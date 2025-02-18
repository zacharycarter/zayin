/* thread_context.c */
#include "thread_context.h"

// Define the thread-local variable.
__thread thread_context_t *current_ctx = NULL;
