/* queue_thunk.h */
#ifndef QUEUE_THUNK_H
#define QUEUE_THUNK_H

#include <mimalloc.h>
#include <stdlib.h>
#include "base.h"

typedef struct queue_thunk {
    size_t len;
    size_t head, tail;
    struct thunk **data;
} queue_thunk;

// Create a new queue.
static inline queue_thunk queue_thunk_new(size_t initial) {
    if (initial < 1) { initial = 1; }
    queue_thunk q;
    q.len = initial;
    q.head = 0;
    q.tail = 0;
    q.data = mi_malloc(q.len * sizeof(struct thunk *));
    return q;
}

// Enqueue a thunk.
static inline void queue_thunk_enqueue(queue_thunk *q, struct thunk *t) {
    q->data[q->head++] = t;
    q->head %= q->len;
    if (q->head == q->tail) {
        size_t old_len = q->len;
        size_t new_len = old_len + (old_len >> 1) + 1;
        q->data = mi_realloc(q->data, new_len * sizeof(struct thunk *));
        q->len = new_len;
    }
}

// Dequeue a thunk.
static inline struct thunk *queue_thunk_dequeue(queue_thunk *q) {
    struct thunk *t = q->data[q->tail++];
    q->tail %= q->len;
    return t;
}

// Get current length.
static inline size_t queue_thunk_len(queue_thunk *q) {
    if (q->head >= q->tail) {
        return q->head - q->tail;
    } else {
        return q->head + q->len - q->tail;
    }
}

#endif // QUEUE_THUNK_H
