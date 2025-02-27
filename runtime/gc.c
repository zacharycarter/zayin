#include <assert.h>
#include <mimalloc.h>
#include <pthread.h>
#include <stdbool.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#include "base.h"
#include "common.h"
#include "gc.h"
#include "hash_table.h"
#include "queue.h"
#include "thread_context.h"
#include "vec.h"

MAKE_VECTOR(size_t, size_t);
MAKE_QUEUE(struct obj *, gc_grey_nodes);
MAKE_QUEUE(struct ptr_toupdate_pair, ptr_toupdate_pair);

/* C89 compatibility: Forward declare all functions */
/* Forward declare all functions */
static gc_heap_t *gc_create_heap(gc_region_t region, size_t initial_size, int thread_index);
static void *gc_heap_allocate(gc_heap_t *heap, size_t size);
static void gc_collect_nursery(struct gc_context *ctx, bool evacuate_all);
static void gc_collect_local_heap(struct gc_context *ctx);
static void gc_collect_shared_heap(void);
static bool gc_try_allocate_tlab(thread_context_t *ctx, size_t size);
static void gc_destroy_heap(gc_heap_t *heap);
static uint64_t get_current_time_ms(void);

/* Constants for memory management */
#define DEFAULT_NURSERY_SIZE (1 * 1024 * 1024)    /* 1MB nursery per thread */
#define DEFAULT_LOCAL_HEAP_SIZE (4 * 1024 * 1024) /* 4MB local heap per thread */
#define DEFAULT_SHARED_HEAP_SIZE (32 * 1024 * 1024) /* 32MB shared heap */
#define DEFAULT_TLAB_SIZE (128 * 1024)            /* 128KB TLAB */

/* Constants for GC tuning */
#define GC_PROMOTION_AGE 3       /* Number of nursery collections before promotion */
#define GC_NURSERY_THRESHOLD 0.8 /* Trigger nursery GC when 80% full */

/* Global GC state */
gc_global_state_t gc_global_state;

/* Array of GC functions for each object type */
static struct gc_funcs gc_func_map[] = {
    [OBJ_CLOSURE] = (struct gc_funcs){.toheap = toheap_closure,
                                      .mark = mark_closure,
                                      .free = gc_free_noop},
    [ENV_OBJ] = (struct gc_funcs){.toheap = toheap_env,
                                  .mark = mark_env,
                                  .free = gc_free_noop},
    [OBJ_CELL] = (struct gc_funcs){.toheap = toheap_cell,
                                   .mark = mark_cell,
                                   .free = gc_free_noop},
    [OBJ_CONS] = (struct gc_funcs){.toheap = toheap_cons,
                                   .mark = mark_cons,
                                   .free = gc_free_noop},
    [OBJ_INT] = (struct gc_funcs){.toheap = toheap_int_obj,
                                  .mark = gc_mark_noop,
                                  .free = gc_free_noop},

    [OBJ_STR] =
        (struct gc_funcs){
            .toheap = toheap_string_obj,
            .mark = gc_mark_noop,
            .free = gc_free_noop,
        },
    [OBJ_HT] = (struct gc_funcs){.toheap = toheap_ht,
                                 .mark = mark_ht,
                                 .free = free_ht},
    [OBJ_BOOL] = (struct gc_funcs){.toheap = toheap_bool_obj,
                                  .mark = gc_mark_noop,
                                  .free = gc_free_noop},
};

/* Helper function to get current time in milliseconds */
static uint64_t get_current_time_ms(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return ((uint64_t)tv.tv_sec * 1000) + ((uint64_t)tv.tv_usec / 1000);
}

/* Initialize the global GC system */
void gc_global_init(void) {
    DEBUG_FPRINTF(stderr, "Initializing global GC system\n");

    /* Initialize the global GC state */
    memset(&gc_global_state, 0, sizeof(gc_global_state_t));

    /* Initialize synchronization primitives */
    pthread_mutex_init(&gc_global_state.gc_mutex, NULL);
    pthread_cond_init(&gc_global_state.gc_cond, NULL);

    /* Create the shared heap */
    gc_global_state.shared_heap = gc_create_heap(
        GC_REGION_SHARED, DEFAULT_SHARED_HEAP_SIZE, -1);

    DEBUG_FPRINTF(stderr, "Global GC system initialized\n");
}

/* Initialize thread-local GC state */
void gc_thread_init(int thread_index) {
    thread_context_t *ctx;

    DEBUG_FPRINTF(stderr, "Initializing GC for thread %d\n", thread_index);

    /* Get the current thread context */
    ctx = get_current_thread_context();
    ctx->thread_index = thread_index;

    /* Initialize thread-local heaps */
    ctx->gc_state.nursery = gc_create_heap(
        GC_REGION_NURSERY, DEFAULT_NURSERY_SIZE, thread_index);

    ctx->gc_state.local_mature = gc_create_heap(
        GC_REGION_LOCAL, DEFAULT_LOCAL_HEAP_SIZE, thread_index);

    /* Initialize thread-local allocation buffer (TLAB) */
    ctx->gc_state.tlab_size = DEFAULT_TLAB_SIZE;
    gc_try_allocate_tlab(ctx, DEFAULT_TLAB_SIZE);

    /* Initialize thread-local GC context */
    ctx->gc_state.gc_ctx = gc_make_context();
    ctx->gc_state.gc_ctx->thread_ctx = ctx;

    /* Increment active thread count */
    pthread_mutex_lock(&gc_global_state.gc_mutex);
    gc_global_state.num_threads++;
    gc_global_state.active_threads++;
    pthread_mutex_unlock(&gc_global_state.gc_mutex);

    DEBUG_FPRINTF(stderr, "GC initialized for thread %d\n", thread_index);
}

/* Clean up thread-local GC state */
void gc_thread_cleanup(void) {
    thread_context_t *ctx = get_current_thread_context();
    if (!ctx) return;

    DEBUG_FPRINTF(stderr, "Cleaning up GC for thread %d\n", ctx->thread_index);

    /* Make sure we're not in the middle of a GC */
    if (ctx->in_gc) {
        DEBUG_FPRINTF(stderr, "Warning: Thread %d is being cleaned up while in GC\n",
                     ctx->thread_index);
    }

    /* Free the thread-local GC context */
    if (ctx->gc_state.gc_ctx) {
        gc_free_context(ctx->gc_state.gc_ctx);
        ctx->gc_state.gc_ctx = NULL;
    }

    /* Destroy thread-local heaps */
    if (ctx->gc_state.nursery) {
        gc_destroy_heap(ctx->gc_state.nursery);
        ctx->gc_state.nursery = NULL;
    }

    if (ctx->gc_state.local_mature) {
        gc_destroy_heap(ctx->gc_state.local_mature);
        ctx->gc_state.local_mature = NULL;
    }

    /* Update global thread count */
    pthread_mutex_lock(&gc_global_state.gc_mutex);
    gc_global_state.active_threads--;
    pthread_mutex_unlock(&gc_global_state.gc_mutex);

    DEBUG_FPRINTF(stderr, "GC cleaned up for thread %d\n", ctx->thread_index);
}

/* Create a new GC heap of the specified type */
static gc_heap_t *gc_create_heap(gc_region_t region, size_t initial_size, int thread_index) {
    gc_heap_t *heap;
    gc_segment_t *segment;

    heap = mi_malloc(sizeof(gc_heap_t));
    memset(heap, 0, sizeof(gc_heap_t));

    heap->region = region;
    heap->owner_thread = thread_index;
    heap->total_size = 0;
    heap->allocated_size = 0;

    /* Initialize heap parameters based on region type */
    switch (region) {
        case GC_REGION_NURSERY:
            /* For nursery, create from-space and to-space for copying collection */
            heap->space_size = initial_size / 2;
            heap->from_space = mi_malloc(heap->space_size);
            heap->to_space = mi_malloc(heap->space_size);
            heap->total_size = initial_size;
            heap->min_size = initial_size;
            heap->max_size = initial_size * 4; /* Allow nursery to grow up to 4x */
            break;

        case GC_REGION_LOCAL:
        case GC_REGION_SHARED:
            /* For mature heaps, we use a mark-sweep approach */

            /* Allocate an initial segment */
            segment = mi_malloc(sizeof(gc_segment_t));
            segment->start = mi_malloc(initial_size);
            segment->size = initial_size;
            segment->next = NULL;
            heap->segments = segment;
            heap->total_size = initial_size;

            /* Create mark bits (1 bit per 8 bytes of heap) */
            heap->num_mark_bits = (initial_size / 8) + 1;
            heap->mark_bits = mi_calloc(heap->num_mark_bits / 8 + 1, 1);

            /* Initialize the free list with the entire segment */
            heap->free_list = (gc_block_t*)segment->start;
            heap->free_list->size = initial_size;
            heap->free_list->next = NULL;

            /* Initialize heap size limits */
            heap->min_size = initial_size;
            heap->max_size = (region == GC_REGION_LOCAL) ?
                initial_size * 4 : /* Local heap can grow up to 4x */
                initial_size * 16;  /* Shared heap can grow up to 16x */

            /* Initialize mutex for shared heap */
            if (region == GC_REGION_SHARED) {
                pthread_mutex_init(&heap->mutex, NULL);
            }
            break;
    }

    return heap;
}

/* Destroy a GC heap and free all memory */
static void gc_destroy_heap(gc_heap_t *heap) {
    gc_segment_t *segment, *next;

    if (!heap) return;

    switch (heap->region) {
        case GC_REGION_NURSERY:
            if (heap->from_space) mi_free(heap->from_space);
            if (heap->to_space) mi_free(heap->to_space);
            break;

        case GC_REGION_LOCAL:
        case GC_REGION_SHARED:
            /* Free all segments */
            segment = heap->segments;
            while (segment) {
                next = segment->next;
                mi_free(segment->start);
                mi_free(segment);
                segment = next;
            }

            /* Free mark bits */
            if (heap->mark_bits) mi_free(heap->mark_bits);

            /* Destroy mutex for shared heap */
            if (heap->region == GC_REGION_SHARED) {
                pthread_mutex_destroy(&heap->mutex);
            }
            break;
    }

    mi_free(heap);
}

/* Allocate memory from a heap */
static void *gc_heap_allocate(gc_heap_t *heap, size_t size) {
    thread_context_t *ctx;
    struct gc_context *gc_ctx;
    gc_block_t **prev_ptr, *block, *new_block;
    size_t new_segment_size;
    gc_segment_t *new_segment;
    uint8_t *new_mark_bits;
    size_t new_mark_bits_size;
    void *result;

    /* Align size to 8 bytes */
    size = (size + 7) & ~7;

    /* For nursery, just bump the pointer */
    if (heap->region == GC_REGION_NURSERY) {
        /* Ensure we have enough space in the nursery */
        ctx = get_current_thread_context();

        /* If allocation would exceed nursery space, run a nursery collection */
        if (ctx->gc_state.tlab_current + size > ctx->gc_state.tlab_end) {
            if (!gc_try_allocate_tlab(ctx, size)) {
                /* If we can't allocate a new TLAB, run a collection */
                DEBUG_FPRINTF(stderr, "Thread %d nursery full, running collection\n",
                             ctx->thread_index);

                gc_ctx = ctx->gc_state.gc_ctx;
                gc_collect_nursery(gc_ctx, false);

                /* Try to allocate a new TLAB after collection */
                if (!gc_try_allocate_tlab(ctx, size)) {
                    /* If still can't allocate, try allocating directly in the local mature heap */
                    DEBUG_FPRINTF(stderr, "Thread %d can't allocate in nursery even after GC, "
                                 "allocating in local mature heap\n", ctx->thread_index);
                    return gc_heap_allocate(ctx->gc_state.local_mature, size);
                }
            }
        }

        /* Allocate from TLAB */
        result = ctx->gc_state.tlab_current;
        ctx->gc_state.tlab_current = (char*)ctx->gc_state.tlab_current + size;

        /* Update statistics */
        ctx->gc_state.bytes_allocated += size;

        return result;
    }

    /* For mature heaps (local and shared), use the free list */

    /* Acquire mutex for shared heap */
    if (heap->region == GC_REGION_SHARED) {
        pthread_mutex_lock(&heap->mutex);
    }

    /* Find a suitable block in the free list */
    prev_ptr = &heap->free_list;
    block = heap->free_list;

    while (block) {
        if (block->size >= size) {
            /* We found a block that's big enough */

            /* If the block is much larger than what we need, split it */
            if (block->size >= size + sizeof(gc_block_t) + 16) {
                new_block = (gc_block_t*)((char*)block + size);
                new_block->size = block->size - size;
                new_block->next = block->next;
                *prev_ptr = new_block;

                /* Update heap statistics */
                heap->allocated_size += size;

                /* Release mutex for shared heap */
                if (heap->region == GC_REGION_SHARED) {
                    pthread_mutex_unlock(&heap->mutex);
                }

                return block;
            } else {
                /* Use the entire block */
                *prev_ptr = block->next;

                /* Update heap statistics */
                heap->allocated_size += block->size;

                /* Release mutex for shared heap */
                if (heap->region == GC_REGION_SHARED) {
                    pthread_mutex_unlock(&heap->mutex);
                }

                return block;
            }
        }

        prev_ptr = &block->next;
        block = block->next;
    }

    /* No suitable block found, need to expand the heap or run GC */

    /* Check if we need to run a collection before expanding */
    if (heap->allocated_size > heap->total_size * 0.8) {
        /* Release mutex for shared heap before collection */
        if (heap->region == GC_REGION_SHARED) {
            pthread_mutex_unlock(&heap->mutex);
        }

        /* Run appropriate collection based on heap type */
        if (heap->region == GC_REGION_LOCAL) {
            ctx = get_current_thread_context();
            gc_collect_local_heap(ctx->gc_state.gc_ctx);
        } else if (heap->region == GC_REGION_SHARED) {
            gc_collect_shared_heap();
        }

        /* Re-acquire mutex for shared heap */
        if (heap->region == GC_REGION_SHARED) {
            pthread_mutex_lock(&heap->mutex);
        }

        /* Try allocation again after collection */
        prev_ptr = &heap->free_list;
        block = heap->free_list;

        while (block) {
            if (block->size >= size) {
                /* We found a block that's big enough after collection */

                /* If the block is much larger than what we need, split it */
                if (block->size >= size + sizeof(gc_block_t) + 16) {
                    new_block = (gc_block_t*)((char*)block + size);
                    new_block->size = block->size - size;
                    new_block->next = block->next;
                    *prev_ptr = new_block;

                    /* Update heap statistics */
                    heap->allocated_size += size;

                    /* Release mutex for shared heap */
                    if (heap->region == GC_REGION_SHARED) {
                        pthread_mutex_unlock(&heap->mutex);
                    }

                    return block;
                } else {
                    /* Use the entire block */
                    *prev_ptr = block->next;

                    /* Update heap statistics */
                    heap->allocated_size += block->size;

                    /* Release mutex for shared heap */
                    if (heap->region == GC_REGION_SHARED) {
                        pthread_mutex_unlock(&heap->mutex);
                    }

                    return block;
                }
            }

            prev_ptr = &block->next;
            block = block->next;
        }
    }

    /* If we get here, we still couldn't find a suitable block after collection */
    /* Try to expand the heap if we haven't reached the maximum size */

    if (heap->total_size < heap->max_size) {
        /* Calculate new segment size (at least as large as the requested allocation) */
        new_segment_size = heap->total_size / 2;
        if (new_segment_size < size) {
            new_segment_size = size;
        }

        /* Ensure we don't exceed max heap size */
        if (heap->total_size + new_segment_size > heap->max_size) {
            new_segment_size = heap->max_size - heap->total_size;
        }

        /* Allocate new segment */
        new_segment = mi_malloc(sizeof(gc_segment_t));
        new_segment->start = mi_malloc(new_segment_size);
        new_segment->size = new_segment_size;
        new_segment->next = heap->segments;
        heap->segments = new_segment;

        /* Update heap size */
        heap->total_size += new_segment_size;

        /* Create a free block for the new segment */
        new_block = (gc_block_t*)new_segment->start;
        new_block->size = new_segment_size;
        new_block->next = heap->free_list;
        heap->free_list = new_block;

        /* Expand mark bits array if needed */
        new_mark_bits_size = (heap->total_size / 8) + 1;
        if (new_mark_bits_size > heap->num_mark_bits) {
            new_mark_bits = mi_calloc(new_mark_bits_size / 8 + 1, 1);
            memcpy(new_mark_bits, heap->mark_bits, heap->num_mark_bits / 8 + 1);
            mi_free(heap->mark_bits);
            heap->mark_bits = new_mark_bits;
            heap->num_mark_bits = new_mark_bits_size;
        }

        /* Now try allocation again */
        if (heap->region == GC_REGION_SHARED) {
            pthread_mutex_unlock(&heap->mutex);
        }
        return gc_heap_allocate(heap, size);
    }

    /* If we get here, we've exhausted all options */
    /* For shared heap, as a last resort, run a full GC */
    if (heap->region == GC_REGION_SHARED) {
        pthread_mutex_unlock(&heap->mutex);

        /* Run a full GC with the low memory flag */
        gc_full(GC_FLAG_FORCE_MAJOR | GC_FLAG_LOW_MEMORY);

        /* Try one more time */
        return gc_heap_allocate(heap, size);
    }

    /* If we get here, allocation has failed */
    if (heap->region == GC_REGION_SHARED) {
        pthread_mutex_unlock(&heap->mutex);
    }

    fprintf(stderr, "Out of memory: Failed to allocate %zu bytes\n", size);
    abort();
    return NULL;
}

/* Try to allocate a new thread-local allocation buffer (TLAB) */
static bool gc_try_allocate_tlab(thread_context_t *ctx, size_t min_size) {
    gc_heap_t *nursery;
    size_t tlab_size;
    void *nursery_end, *nursery_current;

    nursery = ctx->gc_state.nursery;

    /* Calculate TLAB size (at least as large as the minimum required size) */
    tlab_size = ctx->gc_state.tlab_size;
    if (tlab_size < min_size) {
        tlab_size = min_size;
    }

    /* Check if we have enough space in the nursery's from-space */
    nursery_end = (char*)nursery->from_space + nursery->space_size;
    nursery_current = ctx->gc_state.tlab_current;

    if (nursery_current == NULL) {
        /* First allocation, start at the beginning of from-space */
        nursery_current = nursery->from_space;
    } else if ((char*)nursery_current + tlab_size > (char*)nursery_end) {
        /* Not enough space in the nursery */
        return false;
    }

    /* Allocate the TLAB */
    ctx->gc_state.tlab_start = nursery_current;
    ctx->gc_state.tlab_current = nursery_current;
    ctx->gc_state.tlab_end = (char*)nursery_current + tlab_size;

    return true;
}

/* Allocate memory for an object */
void *gc_malloc(size_t size) {
    thread_context_t *ctx = get_current_thread_context();

    /* Try to allocate in the nursery first (via TLAB) */
    if (ctx && ctx->gc_state.nursery) {
        return gc_heap_allocate(ctx->gc_state.nursery, size);
    }

    /* Fallback to shared heap if thread context not initialized */
    return gc_heap_allocate(gc_global_state.shared_heap, size);
}

/* Allocate memory in a specific region */
void *gc_malloc_in_region(size_t size, gc_region_t region) {
    thread_context_t *ctx = get_current_thread_context();

    switch (region) {
        case GC_REGION_NURSERY:
            if (ctx && ctx->gc_state.nursery) {
                return gc_heap_allocate(ctx->gc_state.nursery, size);
            }
            break;

        case GC_REGION_LOCAL:
            if (ctx && ctx->gc_state.local_mature) {
                return gc_heap_allocate(ctx->gc_state.local_mature, size);
            }
            break;

        case GC_REGION_SHARED:
            return gc_heap_allocate(gc_global_state.shared_heap, size);
    }

    /* Fallback to normal allocation */
    return gc_malloc(size);
}

/* Create a new GC context */
struct gc_context *gc_make_context(void) {
    struct gc_context *ctx = mi_malloc(sizeof(struct gc_context));
    ctx->grey_nodes = queue_gc_grey_nodes_new(10);
    ctx->pointers_toupdate = queue_ptr_toupdate_pair_new(10);
    ctx->updated_pointers = hash_table_ptr_map_new();
    ctx->flags = GC_FLAG_NONE;
    ctx->thread_ctx = NULL;

    return ctx;
}

/* Free a GC context */
void gc_free_context(struct gc_context *ctx) {
    if (!ctx) return;

    queue_gc_grey_nodes_free(&ctx->grey_nodes);
    queue_ptr_toupdate_pair_free(&ctx->pointers_toupdate);
    hash_table_ptr_map_free(ctx->updated_pointers);
    mi_free(ctx);
}

/* Mark an object during collection */
void gc_mark_obj(struct gc_context *ctx, struct obj *obj) {
    if (!obj) return;

    /* Mark the object as black (fully processed) */
    obj->mark = BLACK;

    /* Call the appropriate mark function for the object type */
    gc_func_map[obj->tag].mark(obj, ctx);
}

/* Run a minor GC (collect nursery only) */
void gc_minor(struct gc_context *ctx, struct thunk *thnk) {
    thread_context_t *thread_ctx;
    uint64_t start_time, end_time;
    struct obj **maybe_copied;
    struct ptr_toupdate_pair to_update;
    struct obj *on_heap;

    thread_ctx = ctx->thread_ctx;
    if (!thread_ctx) {
        thread_ctx = get_current_thread_context();
        ctx->thread_ctx = thread_ctx;
    }

    DEBUG_FPRINTF(stderr, "Thread %d: Starting minor GC\n", thread_ctx->thread_index);

    /* Mark that we're in GC */
    thread_ctx->in_gc = true;

    /* Track GC start time */
    start_time = get_current_time_ms();

    /* First, ensure the thunk is moved to the heap */
    thnk->closr = (struct closure_obj *)gc_toheap(ctx, (struct obj *)thnk->closr);

    /* Process arguments based on closure size */
    switch (thnk->closr->size) {
        case CLOSURE_ONE:
            if (thnk->one.rand != NULL) {
                thnk->one.rand = gc_toheap(ctx, thnk->one.rand);
            }
            break;

        case CLOSURE_TWO:
            if (thnk->two.rand != NULL) {
                thnk->two.rand = gc_toheap(ctx, thnk->two.rand);
            }
            if (thnk->two.cont != NULL) {
                thnk->two.cont = gc_toheap(ctx, thnk->two.cont);
            }
            break;
    }

    /* Process all pending pointer updates */
    while (queue_ptr_toupdate_pair_len(&ctx->pointers_toupdate) > 0) {
        to_update = queue_ptr_toupdate_pair_dequeue(&ctx->pointers_toupdate);

        maybe_copied = hash_table_ptr_map_lookup(
            ctx->updated_pointers, (size_t)to_update.on_stack);

        if (maybe_copied != NULL) {
            /* We've already updated this pointer, just update the pointer that
               needs to be updated */
            *to_update.toupdate = *maybe_copied;
        } else {
            /* We haven't seen this yet, perform a copy and update */
            assert(to_update.on_stack != NULL);

            on_heap = gc_toheap(ctx, to_update.on_stack);
            hash_table_ptr_map_insert(ctx->updated_pointers,
                                     (size_t)to_update.on_stack, on_heap);
            *to_update.toupdate = on_heap;
        }
    }

    /* Update statistics */
    end_time = get_current_time_ms();

    DEBUG_FPRINTF(stderr, "Thread %d: Minor GC completed in %llu ms\n",
                 thread_ctx->thread_index, (unsigned long long)(end_time - start_time));

    /* Mark that we're no longer in GC */
    thread_ctx->in_gc = false;

    /* Increment collection counter */
    thread_ctx->gc_state.collections++;
}

/* Actual nursery collection implementation */
static void gc_collect_nursery(struct gc_context *ctx, bool evacuate_all) {
    thread_context_t *thread_ctx;
    void *temp;

    thread_ctx = ctx->thread_ctx;
    if (!thread_ctx) {
        thread_ctx = get_current_thread_context();
        ctx->thread_ctx = thread_ctx;
    }

    /* Swap from-space and to-space */
    temp = thread_ctx->gc_state.nursery->from_space;
    thread_ctx->gc_state.nursery->from_space = thread_ctx->gc_state.nursery->to_space;
    thread_ctx->gc_state.nursery->to_space = temp;

    /* Reset TLAB pointers */
    thread_ctx->gc_state.tlab_start = thread_ctx->gc_state.nursery->from_space;
    thread_ctx->gc_state.tlab_current = thread_ctx->gc_state.nursery->from_space;
    thread_ctx->gc_state.tlab_end =
        (char*)thread_ctx->gc_state.nursery->from_space + thread_ctx->gc_state.tlab_size;

    /* TODO: Implement copying collection from to-space to from-space */
    /* This requires tracing all roots and copying live objects */

    /* For now, we just reset the nursery as a simple implementation */
    memset(thread_ctx->gc_state.nursery->to_space, 0, thread_ctx->gc_state.nursery->space_size);
}

/* Run a major GC (collect thread-local mature heap) */
static void gc_collect_local_heap(struct gc_context *ctx) {
    thread_context_t *thread_ctx;
    uint64_t start_time, end_time;
    struct obj *obj;

    thread_ctx = ctx->thread_ctx;
    if (!thread_ctx) {
        thread_ctx = get_current_thread_context();
        ctx->thread_ctx = thread_ctx;
    }

    DEBUG_FPRINTF(stderr, "Thread %d: Starting local heap collection\n",
                 thread_ctx->thread_index);

    /* Mark that we're in GC */
    thread_ctx->in_gc = 1; /* true */

    /* Track GC start time */
    start_time = get_current_time_ms();

    /* Clear mark bits */
    memset(thread_ctx->gc_state.local_mature->mark_bits, 0,
          thread_ctx->gc_state.local_mature->num_mark_bits / 8 + 1);

    /* First, do a nursery collection to ensure young objects are properly handled */
    gc_collect_nursery(ctx, 1); /* true for evacuate_all */

    /* Mark all roots */
    gc_mark_roots(ctx);

    /* Mark phase: Process all grey objects until no more remain */
    while (queue_gc_grey_nodes_len(&ctx->grey_nodes) > 0) {
        obj = queue_gc_grey_nodes_dequeue(&ctx->grey_nodes);
        gc_mark_obj(ctx, obj);
    }

    /* Sweep phase: Free unmarked objects */
    /* TODO: Implement sweep phase for mark-sweep collection */

    /* Reset the free list to prepare for sweep */
    thread_ctx->gc_state.local_mature->free_list = NULL;

    /* TODO: Iterate through all segments and all objects, freeing unmarked ones
       and rebuilding the free list */

    /* Update statistics */
    end_time = get_current_time_ms();

    DEBUG_FPRINTF(stderr, "Thread %d: Local heap collection completed in %llu ms\n",
                 thread_ctx->thread_index, (unsigned long long)(end_time - start_time));

    /* Mark that we're no longer in GC */
    thread_ctx->in_gc = 0; /* false */
}

/* Run a shared heap collection */
static void gc_collect_shared_heap(void) {
    uint64_t start_time, end_time;
    int threads_to_wait_for;

    DEBUG_FPRINTF(stderr, "Starting shared heap collection\n");

    /* Acquire the shared heap mutex */
    pthread_mutex_lock(&gc_global_state.shared_heap->mutex);

    /* If collection is already in progress, wait for it to complete */
    if (gc_global_state.shared_heap->in_collection) {
        pthread_mutex_unlock(&gc_global_state.shared_heap->mutex);

        /* Wait for the collection to complete */
        pthread_mutex_lock(&gc_global_state.gc_mutex);
        while (gc_global_state.shared_heap->in_collection) {
            pthread_cond_wait(&gc_global_state.gc_cond, &gc_global_state.gc_mutex);
        }
        pthread_mutex_unlock(&gc_global_state.gc_mutex);

        return;
    }

    /* Mark that collection is in progress */
    gc_global_state.shared_heap->in_collection = true;

    /* Release the shared heap mutex */
    pthread_mutex_unlock(&gc_global_state.shared_heap->mutex);

    /* Perform stop-the-world phase */
    pthread_mutex_lock(&gc_global_state.gc_mutex);
    gc_global_state.stop_the_world = true;

    /* Wait for all threads to stop */
    threads_to_wait_for = gc_global_state.active_threads;
    while (gc_global_state.gc_threads_waiting < threads_to_wait_for) {
        /* TODO: Implement thread stopping mechanism */
        /* For now, assume threads will cooperate and stop when requested */
        usleep(1000); /* 1ms sleep */
    }

    /* Track GC start time */
    start_time = get_current_time_ms();

    /* Clear mark bits */
    memset(gc_global_state.shared_heap->mark_bits, 0,
          gc_global_state.shared_heap->num_mark_bits / 8 + 1);

    /* TODO: Implement mark phase for shared heap */
    /* This requires tracing all roots from all threads */

    /* TODO: Implement sweep phase for shared heap */

    /* Reset the free list to prepare for sweep */
    gc_global_state.shared_heap->free_list = NULL;

    /* TODO: Iterate through all segments and all objects, freeing unmarked ones
       and rebuilding the free list */

    /* Update statistics */
    end_time = get_current_time_ms();
    gc_global_state.total_collections++;
    gc_global_state.last_collection_time = end_time - start_time;

    DEBUG_FPRINTF(stderr, "Shared heap collection completed in %llu ms\n",
                 (unsigned long long)(end_time - start_time));

    /* End stop-the-world phase */
    gc_global_state.stop_the_world = false;
    gc_global_state.gc_threads_waiting = 0;

    /* Notify waiting threads */
    pthread_cond_broadcast(&gc_global_state.gc_cond);
    pthread_mutex_unlock(&gc_global_state.gc_mutex);

    /* Mark that collection is no longer in progress */
    pthread_mutex_lock(&gc_global_state.shared_heap->mutex);
    gc_global_state.shared_heap->in_collection = false;
    pthread_mutex_unlock(&gc_global_state.shared_heap->mutex);
}

/* Run a full GC (collect all threads and shared heap) */
void gc_full(gc_flags_t flags) {
    uint64_t start_time, end_time;
    int threads_to_wait_for;
    thread_context_t *ctx;
    int i;

    DEBUG_FPRINTF(stderr, "Starting full GC (flags=%d)\n", flags);

    /* Track start time */
    start_time = get_current_time_ms();

    /* Ensure all threads stop */
    pthread_mutex_lock(&gc_global_state.gc_mutex);
    gc_global_state.stop_the_world = true;

    /* Wait for all threads to acknowledge */
    threads_to_wait_for = gc_global_state.active_threads;
    while (gc_global_state.gc_threads_waiting < threads_to_wait_for) {
        /* TODO: Implement thread stopping mechanism */
        /* For now, assume threads will cooperate and stop */
        usleep(1000); /* 1ms sleep */
    }

    /* Collect nurseries for all threads */
    for (i = 0; i < gc_global_state.num_threads; i++) {
        /* TODO: Access thread contexts for all threads, not just the current one */
        /* For now, we only handle the current thread */
        ctx = get_current_thread_context();
        if (ctx && ctx->thread_index == i) {
            gc_collect_nursery(ctx->gc_state.gc_ctx, true);

            /* Collect local mature heap if requested */
            if (flags & GC_FLAG_FORCE_MAJOR) {
                gc_collect_local_heap(ctx->gc_state.gc_ctx);
            }
        }
    }

    /* Always collect shared heap in a full GC */
    /* Reset marked bits */
    memset(gc_global_state.shared_heap->mark_bits, 0,
          gc_global_state.shared_heap->num_mark_bits / 8 + 1);

    /* TODO: Implement mark and sweep for shared heap */

    /* End stop-the-world phase */
    gc_global_state.stop_the_world = false;
    gc_global_state.gc_threads_waiting = 0;

    /* Notify waiting threads */
    pthread_cond_broadcast(&gc_global_state.gc_cond);
    pthread_mutex_unlock(&gc_global_state.gc_mutex);

    /* Update statistics */
    end_time = get_current_time_ms();
    gc_global_state.total_collections++;
    gc_global_state.last_collection_time = end_time - start_time;

    DEBUG_FPRINTF(stderr, "Full GC completed in %llu ms\n",
                 (unsigned long long)(end_time - start_time));
}

/* Mark all roots for a thread */
void gc_mark_roots(struct gc_context *ctx) {
    thread_context_t *thread_ctx;
    struct thunk *thnk;

    thread_ctx = ctx->thread_ctx;
    if (!thread_ctx) {
        thread_ctx = get_current_thread_context();
        ctx->thread_ctx = thread_ctx;
    }

    /* Mark the current thunk if there is one */
    if (thread_ctx->current_thunk) {
        thnk = thread_ctx->current_thunk;

        /* Mark the closure */
        if (thnk->closr) {
            gc_mark_obj(ctx, (struct obj *)thnk->closr);
        }

        /* Mark arguments based on closure size */
        if (thnk->closr) {
            switch (thnk->closr->size) {
                case CLOSURE_ONE:
                    if (thnk->one.rand) {
                        gc_mark_obj(ctx, thnk->one.rand);
                    }
                    break;

                case CLOSURE_TWO:
                    if (thnk->two.rand) {
                        gc_mark_obj(ctx, thnk->two.rand);
                    }
                    if (thnk->two.cont) {
                        gc_mark_obj(ctx, thnk->two.cont);
                    }
                    break;
            }
        }
    }

    /* TODO: Implement stack scanning to mark all roots on the stack */
    /* This is complex and requires careful implementation */
}

/* Write barrier for inter-generational or inter-thread references */
void gc_write_barrier(struct obj *obj, struct obj **field_ptr, struct obj *new_value) {
    thread_context_t *ctx;

    if (!obj || !field_ptr || !new_value) return;

    /* Check if we're writing a reference from an old object to a young object */
    if (!obj->on_stack && new_value->on_stack) {
        /* This is a reference from an old object to a young object */
        /* We need to record this in the remembered set */

        /* For now, just ensure the young object is promoted to the old generation */
        ctx = get_current_thread_context();
        if (ctx && ctx->gc_state.gc_ctx) {
            *field_ptr = gc_toheap(ctx->gc_state.gc_ctx, new_value);
        }
    }

    /* No need to do anything special for references between objects in the same generation */
    *field_ptr = new_value;
}

/* Free an object explicitly */
void gc_free(void *ptr) {
    struct obj *obj;

    if (!ptr) return;

    obj = (struct obj *)ptr;

    /* Only free objects that are on the heap */
    if (!obj->on_stack) {
        /* Call the appropriate free function for the object type */
        gc_func_map[obj->tag].free(obj);

        /* Now free the object itself */
        mi_free(obj);
    }
}

/* This part of the code is kept from the original implementation */
/* It handles copying objects to the heap */
struct obj *gc_toheap(struct gc_context *ctx, struct obj *obj) {
  if (!obj) {
    return NULL;
  }

  /* if we've already copied this object,
     we know that anything it points to must also be sorted */
  struct obj **maybe_copied =
      hash_table_ptr_map_lookup(ctx->updated_pointers, (size_t)obj);
  if (maybe_copied != NULL) {
    return *maybe_copied;
  }

  if (DEBUG_ONLY(obj->tag > LAST_OBJ_TYPE)) {
    RUNTIME_ERROR("object %p is corrupted\n", (void *)obj);
  }
  DEBUG_FPRINTF(stderr, "copying object of type: %u to heap\n", obj->tag);

  struct obj *new_obj = gc_func_map[obj->tag].toheap(obj, ctx);

  /* mark the object as now being on the heap */
  new_obj->on_stack = false;

  /* Add it to the updated map
     Even if it was on the heap already we still insert
     since we then won't process child objects further */
  hash_table_ptr_map_insert(ctx->updated_pointers, (size_t)obj, new_obj);

  return new_obj;
}

/* Dummy implementations for noop functions */
void gc_free_noop(struct obj *obj) { (void)obj; }
void gc_mark_noop(struct obj *obj, struct gc_context *ctx) { (void)obj; (void)ctx; }

/* Other functions from the original implementation */
bool size_t_eq(size_t a, size_t b) { return a == b; }
MAKE_HASH(size_t, struct obj *, hash_table_default_size_t_hash_fun, size_t_eq, ptr_map);

// Mark an object as grey and add it to the queue of grey nodes 'if' it is not
// already grey or black
static bool maybe_mark_grey_and_queue(struct gc_context *ctx, struct obj *obj) {
  if (DEBUG_ONLY(!obj)) {
    DEBUG_FPRINTF(stderr, "trying to mark NULL!\n");
  }
  switch (obj->mark) {
  case BLACK:
  case GREY:
    return false;
  case WHITE:
    obj->mark = GREY;
    queue_gc_grey_nodes_enqueue(&ctx->grey_nodes, obj);
    return true;
  }
}

struct obj *toheap_ht(struct obj *ht_obj, struct gc_context *ctx) {
  struct ht_obj *ht = (struct ht_obj *)ht_obj;

  if (ht->base.on_stack) {
    TOUCH_OBJECT(ht, "toheap_ht");
    struct ht_obj *heap_ht = gc_malloc(sizeof(struct ht_obj));
    *heap_ht = *ht;
    ht = heap_ht;
  }

  HASH_TABLE_ITER(obj, key, val, ht->ht, {
    if (*key) {
      struct ptr_toupdate_pair pk = {.toupdate = (struct obj **)key,
                                     .on_stack = (struct obj *)*key};
      queue_ptr_toupdate_pair_enqueue(&ctx->pointers_toupdate, pk);
    }
    if (*val) {
      struct ptr_toupdate_pair pv = {.toupdate = (struct obj **)val,
                                     .on_stack = (struct obj *)*val};
      queue_ptr_toupdate_pair_enqueue(&ctx->pointers_toupdate, pv);
    }
  });

  return (struct obj *)ht;
}

void mark_ht(struct obj *ht_obj, struct gc_context *ctx) {
  struct ht_obj *ht = (struct ht_obj *)ht_obj;

  HASH_TABLE_ITER(obj, key, val, ht->ht, {
    if (*key)
      maybe_mark_grey_and_queue(ctx, *key);
    if (*val)
      maybe_mark_grey_and_queue(ctx, *val);
  });
}

void free_ht(struct obj *ht_obj) {
  struct ht_obj *ht = (struct ht_obj *)ht_obj;

  hash_table_obj_free(ht->ht);
}

struct obj *toheap_cons(struct obj *cons_obj, struct gc_context *ctx) {
  struct cons_obj *cons = (struct cons_obj *)cons_obj;

  if (cons->base.on_stack) {
    TOUCH_OBJECT(cons, "toheap_cons");
    struct cons_obj *heap_cons = gc_malloc(sizeof(struct cons_obj));
    *heap_cons = *cons;
    cons = heap_cons;
  }

  if (cons->car) {
    struct ptr_toupdate_pair p = {.toupdate = (struct obj **)&cons->car,
                                  .on_stack = (struct obj *)cons->car};
    queue_ptr_toupdate_pair_enqueue(&ctx->pointers_toupdate, p);
  }

  if (cons->cdr) {
    struct ptr_toupdate_pair p = {.toupdate = (struct obj **)&cons->cdr,
                                  .on_stack = (struct obj *)cons->cdr};
    queue_ptr_toupdate_pair_enqueue(&ctx->pointers_toupdate, p);
  }

  return (struct obj *)cons;
}

void mark_cons(struct obj *cons_obj, struct gc_context *ctx) {
  struct cons_obj *cons = (struct cons_obj *)cons_obj;

  if (cons->car) {
    maybe_mark_grey_and_queue(ctx, cons->car);
  }

  if (cons->cdr) {
    maybe_mark_grey_and_queue(ctx, cons->cdr);
  }
}

struct obj *toheap_cell(struct obj *cell_obj, struct gc_context *ctx) {
  struct cell_obj *cell = (struct cell_obj *)cell_obj;

  if (cell->base.on_stack) {
    TOUCH_OBJECT(cell, "toheap_cell");
    struct cell_obj *heap_cell = gc_malloc(sizeof(struct cell_obj));
    *heap_cell = *cell;
    cell = heap_cell;
  }

  if (cell->val) {
    struct ptr_toupdate_pair p = {.toupdate = (struct obj **)&cell->val,
                                  .on_stack = (struct obj *)cell->val};
    queue_ptr_toupdate_pair_enqueue(&ctx->pointers_toupdate, p);
  }

  return (struct obj *)cell;
}

void mark_cell(struct obj *cell_obj, struct gc_context *ctx) {
  struct cell_obj *cell = (struct cell_obj *)cell_obj;

  if (cell->val) {
    maybe_mark_grey_and_queue(ctx, cell->val);
  }
}

struct obj *toheap_env(struct obj *env_obj, struct gc_context *ctx) {
  struct env_obj *env = (struct env_obj *)env_obj;
  struct env_obj *orig_env = env;

  if (env->base.on_stack) {
    TOUCH_OBJECT(env, "toheap_env");
    struct env_obj *heap_env =
        gc_malloc(sizeof(struct env_obj) + env->len * sizeof(struct obj *));

    heap_env->base = env->base;
    heap_env->len = env->len;
    memset(&heap_env->env, 0, env->len * sizeof(struct obj *));
    env = heap_env;
  }

  for (size_t i = 0; i < env->len; i++) {
    struct obj *obj_ptr = orig_env->env[i];

    if (!obj_ptr)
      continue;

    struct ptr_toupdate_pair p = {.toupdate = &env->env[i],
                                  .on_stack = obj_ptr};
    queue_ptr_toupdate_pair_enqueue(&ctx->pointers_toupdate, p);
  }

  return (struct obj *)env;
}

void mark_env(struct obj *env_obj, struct gc_context *ctx) {
  struct env_obj *env = (struct env_obj *)env_obj;
  for (size_t i = 0; i < env->len; i++) {
    if (!env->env[i])
      continue;
    maybe_mark_grey_and_queue(ctx, env->env[i]);
  }
}

struct obj *toheap_closure(struct obj *obj, struct gc_context *ctx) {
  struct closure_obj *clos = (struct closure_obj *)obj;

  if (obj->on_stack) {
    TOUCH_OBJECT(obj, "toheap_closure");
    struct closure_obj *heap_clos = gc_malloc(sizeof(struct closure_obj));
    memcpy(heap_clos, obj, sizeof(struct closure_obj));
    clos = heap_clos;
  }

  if (clos->env) {
    struct ptr_toupdate_pair p = {.toupdate = (struct obj **)&clos->env,
                                  .on_stack = (struct obj *)clos->env};
    queue_ptr_toupdate_pair_enqueue(&ctx->pointers_toupdate, p);
  }

  return (struct obj *)clos;
}

void mark_closure(struct obj *obj, struct gc_context *ctx) {
  struct closure_obj *clos = (struct closure_obj *)obj;

  if (clos->env) {
    maybe_mark_grey_and_queue(ctx, (struct obj *)clos->env);
  }
}

struct obj *toheap_int_obj(struct obj *obj, struct gc_context *ctx) {
  struct int_obj *intobj = (struct int_obj *)obj;

  if (obj->on_stack) {
    TOUCH_OBJECT(obj, "toheap_int");
    struct int_obj *heap_intobj = gc_malloc(sizeof(struct int_obj));
    memcpy(heap_intobj, intobj, sizeof(struct int_obj));
    intobj = heap_intobj;
  }

  return (struct obj *)intobj;
}

struct obj *toheap_bool_obj(struct obj *obj, struct gc_context *ctx) {
  struct bool_obj *boolobj = (struct bool_obj *)obj;

  if (obj->on_stack) {
    TOUCH_OBJECT(obj, "toheap_bool");
    struct bool_obj *heap_boolobj = gc_malloc(sizeof(struct bool_obj));
    memcpy(heap_boolobj, boolobj, sizeof(struct bool_obj));
    boolobj = heap_boolobj;
  }

  return (struct obj *)boolobj;
}

struct obj *toheap_string_obj(struct obj *obj, struct gc_context *ctx) {
  struct string_obj *strobj = (struct string_obj *)obj;

  if (obj->on_stack) {
    TOUCH_OBJECT(obj, "toheap_string");
    size_t total_size = sizeof(struct string_obj) + strobj->len;

    struct string_obj *heap_stringobj = gc_malloc(total_size);

    memcpy(heap_stringobj, strobj, total_size);

    strobj = heap_stringobj;
  }

  return (struct obj *)strobj;
}
