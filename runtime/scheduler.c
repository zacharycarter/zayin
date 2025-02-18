#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "queue_thunk.h"
#include "scheduler.h"
#include "base.h"    // For stack_ptr, etc.

#define NUM_THREADS 4

// Global work queue for thunks.
queue_thunk global_work_queue;

// Mutex and condition variable protecting the work queue.
pthread_mutex_t work_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_queue_cond = PTHREAD_COND_INITIALIZER;

void schedule_thunk(struct thunk *thnk) {
    pthread_mutex_lock(&work_queue_mutex);
    queue_thunk_enqueue(&global_work_queue, thnk);
    // Signal one waiting worker thread.
    pthread_cond_signal(&work_queue_cond);
    pthread_mutex_unlock(&work_queue_mutex);
}

void *thread_main(void *arg) {
    // (Optionally) Initialize per-thread context here.
    while (1) {
        pthread_mutex_lock(&work_queue_mutex);
        // Wait until there is work in the queue.
        while (queue_thunk_len(&global_work_queue) == 0) {
            pthread_cond_wait(&work_queue_cond, &work_queue_mutex);
        }
        struct thunk *thnk = queue_thunk_dequeue(&global_work_queue);
        pthread_mutex_unlock(&work_queue_mutex);

        // Execute the thunk via the trampoline.
        zayin_start(thnk);
    }
    return NULL;
}

/**
 * @brief Starts the scheduler.
 *
 * This function initializes the work queue and spawns the worker threads.
 * It does not join the threads; it returns immediately so that the main thread
 * can continue to schedule work.
 *
 * @return 0 on success.
 */
int start_scheduler(void) {
    global_work_queue = queue_thunk_new(16);

    pthread_t threads[NUM_THREADS];
    for (int i = 0; i < NUM_THREADS; i++) {
        if (pthread_create(&threads[i], NULL, thread_main, NULL) != 0) {
            perror("pthread_create");
            exit(1);
        }
    }
    // Remove the join loop; since worker threads run indefinitely,
    // joining here would block the main thread forever.
    return 0;
}
