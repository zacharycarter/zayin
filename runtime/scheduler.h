#ifndef SCHEDULER_H
#define SCHEDULER_H

#include "base.h" // Assumes that struct thunk is defined here
#include "queue_thunk.h"  // Provides the queue_thunk type and functions

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Starts the scheduler by creating and launching the worker threads.
 *
 * This function initializes the global work queue and spawns a fixed number of
 * worker threads. Each worker thread will run in a loop, waiting for thunks to
 * be scheduled for execution.
 *
 * @return 0 on success, non-zero on error.
 */
int start_scheduler(void);

/**
 * @brief Schedules a thunk for execution by one of the worker threads.
 *
 * This function enqueues the given thunk into the global work queue. Worker
 * threads will pick up scheduled thunks and execute them.
 *
 * @param thnk A pointer to the thunk to be scheduled.
 */
void schedule_thunk(struct thunk *thnk);

/**
 * @brief Dequeues a thunk from the global work queue.
 *
 * This function removes and returns the next thunk from the work queue. If the
 * queue is empty, it returns NULL.
 *
 * @return A pointer to the dequeued thunk, or NULL if the queue is empty.
 */
struct thunk *dequeue_thunk(void);

#ifdef __cplusplus
}
#endif

#endif // SCHEDULER_H
