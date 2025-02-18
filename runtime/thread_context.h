/* thread_context.h */
#ifndef THREAD_CONTEXT_H
#define THREAD_CONTEXT_H

#include <pthread.h>
#include <setjmp.h>
#include "base.h"  // Assumes your thunk type is declared here

typedef struct thread_context {
    struct thunk *current_thunk;
    void *stack_initial;
    jmp_buf setjmp_env;
    // (Optional) Add fields for per‑thread GC context if needed.
} thread_context_t;

extern __thread thread_context_t *current_ctx;

#endif // THREAD_CONTEXT_H
