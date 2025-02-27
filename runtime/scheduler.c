#include <mimalloc.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "base.h"
#include "common.h"
#include "gc.h"
#include "queue_thunk.h"
#include "scheduler.h"
#include "thread_context.h"

/* Forward declarations */
static void *thread_main(void *arg);
static void scheduler_init(void);
static void check_gc_cooperation(void);

/* Number of worker threads to spawn. */
#define NUM_THREADS 4

/* Global work queue for thunks */
queue_thunk global_work_queue;

/* Mutex and condition variables protecting the work queue. */
pthread_mutex_t work_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_queue_cond = PTHREAD_COND_INITIALIZER;

/* GC cooperation */
pthread_mutex_t gc_coop_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t gc_coop_cond = PTHREAD_COND_INITIALIZER;
int gc_paused_threads = 0;

/* Worker threads */
pthread_t worker_threads[NUM_THREADS];
int active_threads = 0;
int scheduler_running = 0; /* boolean */

/* Initialize the scheduler and global resources */
static void scheduler_init(void) {
    pthread_mutex_lock(&work_queue_mutex);
    if (global_work_queue.data == NULL) {
        global_work_queue = queue_thunk_new(16);
    }
    pthread_mutex_unlock(&work_queue_mutex);

    /* Initialize global GC system */
    gc_global_init();
}

/* Enqueue a thunk for execution by worker threads */
void schedule_thunk(struct thunk *thnk) {
    pthread_mutex_lock(&work_queue_mutex);

    /* Initialize work queue if needed */
    if (global_work_queue.data == NULL) {
        global_work_queue = queue_thunk_new(16);
    }

    /* Add the thunk to the work queue */
    queue_thunk_enqueue(&global_work_queue, thnk);

    /* Signal a waiting worker */
    pthread_cond_signal(&work_queue_cond);
    pthread_mutex_unlock(&work_queue_mutex);
}

/* Dequeue a thunk from the work queue */
struct thunk *dequeue_thunk(void) {
    struct thunk *thnk = NULL;

    pthread_mutex_lock(&work_queue_mutex);

    if (queue_thunk_len(&global_work_queue) > 0) {
        thnk = queue_thunk_dequeue(&global_work_queue);
    }

    pthread_mutex_unlock(&work_queue_mutex);
    return thnk;
}

/* Check if GC needs this thread to pause (stop-the-world cooperation) */
static void check_gc_cooperation(void) {
    thread_context_t *ctx = get_current_thread_context();

    if (!ctx) return;

    /* Quick check without locking to avoid mutex overhead */
    if (__atomic_load_n(&gc_global_state.stop_the_world, __ATOMIC_ACQUIRE)) {
        /* Acquire GC mutex to cooperate with collection */
        pthread_mutex_lock(&gc_global_state.gc_mutex);

        if (gc_global_state.stop_the_world) {
            /* Increment waiting threads count */
            gc_global_state.gc_threads_waiting++;
            DEBUG_FPRINTF(stderr, "Thread %d stopping for GC\n", ctx->thread_index);

            /* Wait for GC to complete */
            ctx->in_gc = 1; /* true */
            while (gc_global_state.stop_the_world) {
                pthread_cond_wait(&gc_global_state.gc_cond, &gc_global_state.gc_mutex);
            }
            ctx->in_gc = 0; /* false */

            DEBUG_FPRINTF(stderr, "Thread %d resuming after GC\n", ctx->thread_index);
        }

        pthread_mutex_unlock(&gc_global_state.gc_mutex);
    }
}

/* Worker thread main function */
static void *thread_main(void *arg) {
    int thread_index = *((int*)arg);
    thread_context_t *ctx;
    struct thunk *thnk;

    free(arg); /* Free the thread index argument */

    /* Initialize thread context with proper thread index */
    thread_context_init(thread_index);

    /* Initialize thread-local GC state */
    gc_thread_init(thread_index);

    DEBUG_FPRINTF(stderr, "Worker thread %d started\n", thread_index);

    /* Main worker loop */
    while (scheduler_running) {
        /* Cooperate with GC if needed */
        check_gc_cooperation();

        /* Try to get a thunk to execute */
        pthread_mutex_lock(&work_queue_mutex);

        /* Wait for work if the queue is empty */
        while (scheduler_running && queue_thunk_len(&global_work_queue) == 0) {
            pthread_cond_wait(&work_queue_cond, &work_queue_mutex);

            /* After waking up, check if we need to cooperate with GC */
            if (__atomic_load_n(&gc_global_state.stop_the_world, __ATOMIC_ACQUIRE)) {
                pthread_mutex_unlock(&work_queue_mutex);
                check_gc_cooperation();
                pthread_mutex_lock(&work_queue_mutex);
            }
        }

        /* Check again if scheduler is still running after waiting */
        if (!scheduler_running) {
            pthread_mutex_unlock(&work_queue_mutex);
            break;
        }

        /* Get a thunk to execute */
        thnk = NULL;
        if (queue_thunk_len(&global_work_queue) > 0) {
            thnk = queue_thunk_dequeue(&global_work_queue);
        }

        pthread_mutex_unlock(&work_queue_mutex);

        /* Execute the thunk if we got one */
        if (thnk) {
            ctx = get_current_thread_context();

            /* Update current thunk in thread context */
            ctx->current_thunk = thnk;

            /* Execute the thunk */
            zayin_start(thnk);

            /* Clear current thunk reference */
            ctx->current_thunk = NULL;
        }
    }

    /* Clean up thread-local resources */
    gc_thread_cleanup();
    thread_context_cleanup();

    DEBUG_FPRINTF(stderr, "Worker thread %d exiting\n", thread_index);
    return NULL;
}

/* Start the scheduler with worker threads */
int start_scheduler(void) {
    int i;
    int *thread_index;

    /* Don't start if already running */
    if (scheduler_running) {
        return 0;
    }

    /* Initialize scheduler resources */
    scheduler_init();

    /* Mark scheduler as running */
    scheduler_running = 1; /* true */

    /* Create worker threads */
    for (i = 0; i < NUM_THREADS; i++) {
        /* Allocate thread index on heap (will be freed by worker) */
        thread_index = malloc(sizeof(int));
        *thread_index = i;

        if (pthread_create(&worker_threads[i], NULL, thread_main, thread_index) != 0) {
            perror("pthread_create");

            /* Clean up and return error */
            scheduler_running = 0; /* false */
            for (i = 0; i < active_threads; i++) {
                pthread_cancel(worker_threads[i]);
                pthread_join(worker_threads[i], NULL);
            }
            return -1;
        }

        active_threads++;
    }

    DEBUG_FPRINTF(stderr, "Scheduler started with %d worker threads\n", NUM_THREADS);
    return 0;
}

/* Stop the scheduler and clean up resources */
int stop_scheduler(void) {
    int i;

    if (!scheduler_running) {
        return 0;
    }

    /* Mark scheduler as stopping */
    scheduler_running = 0; /* false */

    /* Wake up all waiting threads */
    pthread_mutex_lock(&work_queue_mutex);
    pthread_cond_broadcast(&work_queue_cond);
    pthread_mutex_unlock(&work_queue_mutex);

    /* Wait for all threads to exit */
    for (i = 0; i < active_threads; i++) {
        pthread_join(worker_threads[i], NULL);
    }

    /* Reset active threads count */
    active_threads = 0;

    /* Clean up the work queue */
    if (global_work_queue.data != NULL) {
        mi_free(global_work_queue.data);
        global_work_queue.data = NULL;
    }

    DEBUG_FPRINTF(stderr, "Scheduler stopped\n");
    return 0;
}
