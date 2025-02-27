/* thread_context.h */
#ifndef THREAD_CONTEXT_H
#define THREAD_CONTEXT_H

#include <pthread.h>
#include <setjmp.h>
#include "base.h"  // Assumes your thunk type is declared here

// Forward declaration to avoid circular dependencies
struct gc_heap;
struct gc_context;

typedef struct thread_context {
    struct thunk *current_thunk;
    void *stack_initial;
    jmp_buf setjmp_env;

    // Thread-local GC state
    struct {
        // Thread-local nursery (young generation)
        struct gc_heap *nursery;

        // Thread-local mature space (thread-local old generation)
        struct gc_heap *local_mature;

        // Thread-local allocation buffer
        void *tlab_start;
        void *tlab_current;
        void *tlab_end;
        size_t tlab_size;

        // Thread-local GC context for collection operations
        struct gc_context *gc_ctx;

        // Statistics for this thread
        size_t allocations;
        size_t collections;
        size_t bytes_allocated;
        size_t bytes_promoted;
    } gc_state;

    // Thread ID for debugging and synchronization
    pthread_t thread_id;
    int thread_index;

    // Flag to indicate if this thread is currently in GC
    bool in_gc;
} thread_context_t;

extern __thread thread_context_t *current_ctx;

// Initialize a thread context
void thread_context_init(int thread_index);

// Clean up a thread context
void thread_context_cleanup(void);

// Get thread context by thread id
thread_context_t *get_thread_context(int thread_index);

// Get the current thread context, initializing if needed
thread_context_t *get_current_thread_context(void);

#endif // THREAD_CONTEXT_H
