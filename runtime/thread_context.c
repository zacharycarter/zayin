/* thread_context.c */
#include <mimalloc.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "thread_context.h"
#include "gc.h"  /* Will need modification to avoid circular dependencies */

/* Define the thread-local variable. */
__thread thread_context_t *current_ctx = NULL;

/* Default TLAB size - adjust based on your application's needs */
#define DEFAULT_TLAB_SIZE (128 * 1024)  /* 128KB */

/* External function from gc.c - declared here to avoid circular dependency */
extern void gc_thread_cleanup(void);

/* Initialize a thread context */
void thread_context_init(int thread_index) {
    if (current_ctx != NULL) {
        /* Already initialized */
        return;
    }

    /* Allocate and initialize the thread context */
    current_ctx = mi_malloc(sizeof(thread_context_t));
    memset(current_ctx, 0, sizeof(thread_context_t));

    /* Set up thread identification */
    current_ctx->thread_id = pthread_self();
    current_ctx->thread_index = thread_index;
    current_ctx->stack_initial = stack_ptr();

    /* Initialize thread-local GC state */
    /* Note: These will be properly initialized in gc_thread_init() */
    current_ctx->gc_state.tlab_size = DEFAULT_TLAB_SIZE;
    current_ctx->in_gc = 0; /* false */

    /* Log thread creation (in debug mode) */
    DEBUG_FPRINTF(stderr, "Thread %d initialized (id=%lu)\n",
                 thread_index, (unsigned long)current_ctx->thread_id);
}

/* Clean up a thread context */
void thread_context_cleanup(void) {
    if (current_ctx == NULL) {
        return;
    }

    /* Clean up GC resources */
    /* This will be implemented in the updated gc.c */
    gc_thread_cleanup();

    /* Free the thread context */
    mi_free(current_ctx);
    current_ctx = NULL;
}

/* Get the current thread context, initializing if needed */
thread_context_t *get_current_thread_context(void) {
    if (current_ctx == NULL) {
        /* Use thread index 0 as a default for threads not started via the scheduler */
        thread_context_init(0);
    }
    return current_ctx;
}
