#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "base.h"
#include "gc.h"
#include "queue_thunk.h"
#include "scheduler.h"
#include "thread_context.h"

#define NUM_THREADS 4

// Global work queue for thunks
queue_thunk global_work_queue;

// Mutex and condition variable protecting the work queue
pthread_mutex_t work_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_queue_cond = PTHREAD_COND_INITIALIZER;

void schedule_thunk(struct thunk *thnk) {
    pthread_mutex_lock(&work_queue_mutex);
    queue_thunk_enqueue(&global_work_queue, thnk);
    pthread_cond_signal(&work_queue_cond);
    pthread_mutex_unlock(&work_queue_mutex);
}

static void *thread_main(void *arg) {
    // Initialize thread context
    if (current_ctx == NULL) {
        current_ctx = malloc(sizeof(thread_context_t));
    }
    memset(current_ctx, 0, sizeof(thread_context_t));
    current_ctx->stack_initial = stack_ptr();

    while (1) {
        pthread_mutex_lock(&work_queue_mutex);

        // Wait for work
        while (queue_thunk_len(&global_work_queue) == 0) {
            pthread_cond_wait(&work_queue_cond, &work_queue_mutex);
        }

        struct thunk *thnk = queue_thunk_dequeue(&global_work_queue);
        pthread_mutex_unlock(&work_queue_mutex);

        // Update current thunk and execute
        current_ctx->current_thunk = thnk;
        zayin_start(thnk);
    }
    return NULL;
}

int start_scheduler(void) {
    // Initialize work queue and GC
    global_work_queue = queue_thunk_new(16);
    gc_init();

    // Create worker threads
    pthread_t threads[NUM_THREADS];
    for (int i = 0; i < NUM_THREADS; i++) {
        if (pthread_create(&threads[i], NULL, thread_main, NULL) != 0) {
            perror("pthread_create");
            exit(1);
        }
    }

    return 0;
}
