#ifndef SOMESCHEME_GC_H
#define SOMESCHEME_GC_H

#include <stdbool.h>
#include <pthread.h>
#include "base.h"
#include "queue.h"
#include "hash_table.h"
#include "vec.h"

/* Forward declarations to avoid circular dependencies */
struct thread_context;

DEFINE_VECTOR(size_t, size_t);
DEFINE_VECTOR(struct obj *, gc_heap_nodes);
DEFINE_QUEUE(struct obj *, gc_grey_nodes);
DEFINE_HASH(size_t, struct obj *, ptr_map);

/* GC flags for controlling behavior */
typedef enum {
    GC_FLAG_NONE = 0,
    GC_FLAG_FORCE_MAJOR = (1 << 0),   /* Force a major collection */
    GC_FLAG_CONCURRENT = (1 << 1),    /* Perform concurrent collection if possible */
    GC_FLAG_LOW_MEMORY = (1 << 2)     /* System is under memory pressure */
} gc_flags_t;

/* Heap regions for memory management */
typedef enum {
    GC_REGION_NURSERY = 0,  /* Thread-local young generation */
    GC_REGION_LOCAL,        /* Thread-local mature objects */
    GC_REGION_SHARED        /* Shared heap (accessible by all threads) */
} gc_region_t;

/* Memory block that can be allocated from a heap */
typedef struct gc_block {
    size_t size;             /* Size of the block in bytes */
    struct gc_block *next;   /* Next block in the free list */
} gc_block_t;

/* Thread-local allocation buffer (TLAB) */
typedef struct gc_tlab {
    void *start;            /* Start of the TLAB */
    void *current;          /* Current allocation position */
    void *end;              /* End of the TLAB */
    size_t size;            /* Size of the TLAB */
    bool active;            /* Whether this TLAB is currently in use */
} gc_tlab_t;

/* Heap segment representing a contiguous region of memory */
typedef struct gc_segment {
    void *start;                /* Start address of the segment */
    size_t size;                /* Size of the segment in bytes */
    struct gc_segment *next;    /* Next segment in the list */
} gc_segment_t;

/* Heap structure representing a memory region (nursery, local mature, or shared) */
typedef struct gc_heap {
    gc_region_t region;         /* The type of region this heap represents */
    gc_segment_t *segments;     /* List of memory segments in this heap */
    gc_block_t *free_list;      /* List of free blocks for allocation */
    size_t total_size;          /* Total size of all segments */
    size_t allocated_size;      /* Total size of allocated objects */
    size_t min_size;            /* Minimum size to maintain */
    size_t max_size;            /* Maximum size allowed to grow to */
    int owner_thread;           /* Thread index that owns this heap (-1 for shared) */

    /* For nursery (young generation) heaps */
    void *from_space;           /* From-space for copying collection */
    void *to_space;             /* To-space for copying collection */
    size_t space_size;          /* Size of each space */

    /* For mature heaps (local and shared) */
    uint8_t *mark_bits;         /* Bitmap for marking */
    size_t num_mark_bits;       /* Number of bits in the mark bitmap */

    /* Synchronization for shared heap */
    pthread_mutex_t mutex;      /* Mutex for thread synchronization */
    bool in_collection;         /* Whether collection is in progress */
} gc_heap_t;

/* Global GC state */
typedef struct gc_global_state {
    /* Shared heap (accessible by all threads) */
    gc_heap_t *shared_heap;

    /* GC synchronization */
    pthread_mutex_t gc_mutex;       /* Global GC mutex */
    pthread_cond_t gc_cond;         /* Condition variable for GC synchronization */
    int gc_threads_waiting;         /* Number of threads waiting on GC */
    bool stop_the_world;            /* Whether all threads should stop for GC */

    /* Thread coordination */
    int num_threads;                /* Total number of threads */
    int active_threads;             /* Number of active threads */

    /* Statistics */
    size_t total_collections;       /* Total number of collections */
    size_t total_allocated;         /* Total bytes allocated */
    size_t objects_marked;          /* Total number of ojbects makred */
    size_t last_collection_time;    /* Time of last collection (ms) */
    size_t bytes_freed;             /* Number of bytes freed */
} gc_global_state_t;

/* Make the global state available to other modules */
extern gc_global_state_t gc_global_state;

struct ptr_toupdate_pair {
  struct obj **toupdate;
  struct obj *on_stack;
};

DEFINE_QUEUE(struct ptr_toupdate_pair, ptr_toupdate_pair)

/* GC context for a collection operation */
struct gc_context {
  /* nodes that are marked grey */
  struct queue_gc_grey_nodes grey_nodes;

  /* pointers that need to be updated when
   * another pointer has been moved to the heap
   * pair is (pointer_to_update, stack_pointer) */
  struct queue_ptr_toupdate_pair pointers_toupdate;

  /* pointers that have been updated to the heap
   * pair is (stack_pointer, heap_pointer) */
  struct hash_table_ptr_map *updated_pointers;

  /* Collection flags */
  gc_flags_t flags;

  /* Reference to the thread context if this is a thread-local collection */
  struct thread_context *thread_ctx;
};

/* Function signatures for object handling */
struct gc_funcs {
  /* Copies the object to the heap and updates
   * anything it points to to point to the heap
   * if the object is on the heap already this returns the same
   * pointer that was put in */
  struct obj *(*const toheap)(struct obj *, struct gc_context *);

  /* Marks an object and any child pointers
   * Stack objects are copied to the heap and the context updated */
  void (*const mark)(struct obj *, struct gc_context *);

  /* Frees an object
   * Acts as the cleanup routine, the gc will decide whether to call free on
   * the object if it is on the stack or not */
  void (*const free)(struct obj *);
};

/* Initialize the global GC system */
void gc_global_init(void);

/* Initialize thread-local GC state */
void gc_thread_init(int thread_index);

/* Clean up thread-local GC state */
void gc_thread_cleanup(void);

/* Allocate memory for an object */
void *gc_malloc(size_t size);

/* Allocate memory in a specific region */
void *gc_malloc_in_region(size_t size, gc_region_t region);

/* Free an object */
void gc_free(void *ptr);

/* Run a minor GC (collect nursery only) */
void gc_minor(struct gc_context *ctx, struct thunk *thnk);

/* Run a major GC (collect all heaps) */
void gc_major(struct gc_context *ctx, struct thunk *thnk);

/* Run a full GC (collect all threads and shared heap) */
void gc_full(gc_flags_t flags);

/* Create a new GC context */
struct gc_context *gc_make_context(void);

/* Free a GC context */
void gc_free_context(struct gc_context *ctx);

/* Move an object to the heap */
struct obj *gc_toheap(struct gc_context *ctx, struct obj *);

/* Mark an object during collection */
void gc_mark_obj(struct gc_context *ctx, struct obj *);

/* Mark all roots for a thread */
void gc_mark_roots(struct gc_context *ctx);

/* Write barrier for inter-generational or inter-thread references */
void gc_write_barrier(struct obj *obj, struct obj **field_ptr, struct obj *new_value);

/* Thread stopping mechanism - allows threads to check flag and pause execution */
void gc_check_pause_for_collection(void);

/* Various helper functions */
void gc_heap_maintain(void);
void gc_free_noop(struct obj *);
void gc_mark_noop(struct obj *, struct gc_context *);
void gc_reset_stats(void);
void gc_get_stats(size_t *collections, size_t *objects_marked,
                  size_t *bytes_freed, size_t *allocated);

/* Object handling functions (from original implementation) */
struct obj *toheap_closure(struct obj *, struct gc_context *);
void mark_closure(struct obj *, struct gc_context *);

struct obj *toheap_env(struct obj *, struct gc_context *);
void mark_env(struct obj *, struct gc_context *);

struct obj *toheap_int_obj(struct obj *, struct gc_context *);
struct obj *toheap_bool_obj(struct obj *, struct gc_context *);
struct obj *toheap_string_obj(struct obj *, struct gc_context *);

struct obj *toheap_cell(struct obj *, struct gc_context *);
void mark_cell(struct obj *, struct gc_context *);

struct obj *toheap_cons(struct obj *, struct gc_context *);
void mark_cons(struct obj *, struct gc_context *);

struct obj *toheap_ht(struct obj *, struct gc_context *);
void mark_ht(struct obj *, struct gc_context *);
void free_ht(struct obj *);

#endif /* SOMESCHEME_GC_H */
