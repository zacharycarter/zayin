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

/* Maximum number of threads we support */
#define MAX_THREADS 64

/* Default TLAB size - adjust based on your application's needs */
#define DEFAULT_TLAB_SIZE (128 * 1024)  /* 128KB */

/* External function from gc.c - declared here to avoid circular dependency */
extern void gc_thread_cleanup(void);

/* Array of thread contexts */
static thread_context_t *all_thread_contexts[MAX_THREADS] = {NULL};
static pthread_mutex_t context_registry_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Register a thread context when it's created */
void register_thread_context(thread_context_t *ctx) {
    if (!ctx) return;

    pthread_mutex_lock(&context_registry_mutex);
    all_thread_contexts[ctx->thread_index] = ctx;
    pthread_mutex_unlock(&context_registry_mutex);
}

/* Unregister a thread context when it's destroyed */
void unregister_thread_context(thread_context_t *ctx) {
    if (!ctx) return;

    pthread_mutex_lock(&context_registry_mutex);
    all_thread_contexts[ctx->thread_index] = NULL;
    pthread_mutex_unlock(&context_registry_mutex);
}

/* Get a thread context by index */
thread_context_t *get_thread_context(int index) {
    if (index < 0 || index >= MAX_THREADS) return NULL;

    pthread_mutex_lock(&context_registry_mutex);
    thread_context_t *ctx = all_thread_contexts[index];
    pthread_mutex_unlock(&context_registry_mutex);

    return ctx;
}

/* Update thread_context_init to register the context */
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
    current_ctx->gc_state.tlab_size = DEFAULT_TLAB_SIZE;
    current_ctx->in_gc = false;

    /* Register this context in the global registry */
    register_thread_context(current_ctx);

    /* Log thread creation (in debug mode) */
    DEBUG_FPRINTF(stderr, "Thread %d initialized (id=%lu)\n",
                 thread_index, (unsigned long)current_ctx->thread_id);
}

/* Update thread_context_cleanup to unregister the context */
void thread_context_cleanup(void) {
    if (current_ctx == NULL) {
        return;
    }

    /* Unregister from the global registry */
    unregister_thread_context(current_ctx);

    /* Clean up GC resources */
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
