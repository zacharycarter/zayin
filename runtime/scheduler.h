#ifndef SCHEDULER_H
#define SCHEDULER_H

#include "base.h" // Assumes that struct thunk is defined here

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

#ifdef __cplusplus
}
#endif

#endif // SCHEDULER_H
