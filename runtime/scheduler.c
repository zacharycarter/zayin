#include <mimalloc.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "base.h"
#include "gc.h"
#include "queue_thunk.h"   // Your queue implementation for thunks
#include "scheduler.h"
#include "thread_context.h"

// Number of worker threads to spawn.
#define NUM_THREADS 4

// Global work queue for thunks, using your queue_thunk type.
// We now initialize this queue using the MAKE_QUEUE macro from your queue_thunk module.
queue_thunk global_work_queue;

// Mutex and condition variable protecting the work queue.
pthread_mutex_t work_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_queue_cond = PTHREAD_COND_INITIALIZER;

/**
 * scheduler_init
 *
 * Optional helper that ensures the global work queue is properly initialized.
 * This uses your queue_thunk_new() function (generated by your DEFINE_QUEUE/MAKE_QUEUE macros)
 * to allocate a queue with an initial capacity.
 */
static void scheduler_init(void) {
    pthread_mutex_lock(&work_queue_mutex);
    if (global_work_queue.data == NULL) {
        global_work_queue = queue_thunk_new(16);
    }
    pthread_mutex_unlock(&work_queue_mutex);
}

/**
 * schedule_thunk
 *
 * Enqueues a thunk into the global work queue. This implementation uses
 * the queue_thunk_enqueue() function (from your queue_thunk macros) to add
 * the thunk to the queue, and then signals a waiting worker.
 */
void schedule_thunk(struct thunk *thnk) {
    pthread_mutex_lock(&work_queue_mutex);
    // If the work queue hasn't been initialized yet, do so.
    if (global_work_queue.data == NULL) {
        global_work_queue = queue_thunk_new(16);
    }
    queue_thunk_enqueue(&global_work_queue, thnk);
    pthread_cond_signal(&work_queue_cond);
    pthread_mutex_unlock(&work_queue_mutex);
}

/**
 * dequeue_thunk
 *
 * Removes and returns a thunk from the global work queue.
 * Returns NULL if the queue is empty.
 */
struct thunk *dequeue_thunk(void) {
    pthread_mutex_lock(&work_queue_mutex);
    struct thunk *thnk = NULL;
    if (queue_thunk_len(&global_work_queue) > 0) {
        thnk = queue_thunk_dequeue(&global_work_queue);
    }
    pthread_mutex_unlock(&work_queue_mutex);
    return thnk;
}

/**
 * thread_main
 *
 * This is the worker thread function. Each thread initializes its thread
 * context and then repeatedly waits for work. When a thunk is available in the
 * global work queue, it is dequeued and executed via zayin_start().
 */
static void *thread_main(void *arg) {
    // Initialize thread context if not already initialized.
    if (current_ctx == NULL) {
        current_ctx = mi_malloc(sizeof(thread_context_t));
    }
    memset(current_ctx, 0, sizeof(thread_context_t));
    current_ctx->stack_initial = stack_ptr();

    while (1) {
        pthread_mutex_lock(&work_queue_mutex);
        // Wait for work if the queue is empty.
        while (queue_thunk_len(&global_work_queue) == 0) {
            pthread_cond_wait(&work_queue_cond, &work_queue_mutex);
        }
        struct thunk *thnk = queue_thunk_dequeue(&global_work_queue);
        pthread_mutex_unlock(&work_queue_mutex);

        // Update current thunk in the thread context and execute it.
        current_ctx->current_thunk = thnk;
        zayin_start(thnk);
    }
    return NULL;
}

/**
 * start_scheduler
 *
 * Initializes the global work queue and spawns a fixed number of worker threads.
 */
int start_scheduler(void) {
    // Initialize the global work queue.
    scheduler_init();
    // Also initialize GC, if required.
    gc_init();

    // Create worker threads.
    pthread_t threads[NUM_THREADS];
    for (int i = 0; i < NUM_THREADS; i++) {
        if (pthread_create(&threads[i], NULL, thread_main, NULL) != 0) {
            perror("pthread_create");
            exit(1);
        }
    }

    return 0;
}
