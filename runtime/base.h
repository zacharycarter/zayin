#ifndef ZAYIN_H
#define ZAYIN_H

#include <stdbool.h>
#include <stdlib.h>

#include "common.h"
#include "vec.h"
#include "hash_table.h"

#define NUM_ARGS(...) (sizeof((size_t[]){__VA_ARGS__}) / sizeof(size_t))

#define OBJECT_STRING_OBJ_NEW(NAME, S)                                         \
  struct obj *(NAME);                                                          \
  do {                                                                         \
    size_t len = strlen(S) + 1;                                                \
    /* we keep the null byte */                                                \
    struct string_obj *new_obj = alloca(sizeof(struct string_obj) + len);      \
    new_obj->base = object_base_new(OBJ_STR);                                  \
    new_obj->len = len;                                                        \
    memcpy((char *)&new_obj->buf, (S), len);                                   \
    TOUCH_OBJECT(new_obj, "string_obj_new");                                   \
    (NAME) = (struct obj *)new_obj;                                            \
  } while (0)

#define OBJECT_INT_OBJ_NEW(NAME, n)                                            \
  struct obj *(NAME);                                                          \
  do {                                                                         \
    struct int_obj *new_obj = alloca(sizeof(struct int_obj));                  \
    *new_obj = object_int_obj_new((n));                                        \
    TOUCH_OBJECT(new_obj, "int_obj_new");                                      \
    (NAME) = (struct obj *)new_obj;                                            \
  } while (0)

#define OBJECT_BOOL_OBJ_NEW(NAME, b)                                           \
  struct obj *(NAME);                                                          \
  do {                                                                         \
    struct bool_obj *new_obj =                                                 \
        (struct bool_obj*)alloca(sizeof(struct bool_obj));                     \
    *new_obj = object_bool_obj_new((b));                                       \
    TOUCH_OBJECT(new_obj, "bool_obj_new");                                     \
    (NAME) = (struct obj *)new_obj;                                            \
  } while (0)

#define ENV_STRUCT(T)                                                          \
  struct {                                                                     \
    struct obj base;                                                           \
    size_t len;                                                                \
    T env;                                                                     \
  }

#define OBJECT_ENV_OBJ_NEW(NAME, S)                                            \
  struct env_obj *(NAME);                                                      \
  do {                                                                         \
    ENV_STRUCT(S) *new_env = alloca(sizeof(ENV_STRUCT(S)));                    \
    new_env->base = object_base_new(ENV_OBJ);                                  \
    new_env->len = sizeof(S) / sizeof(struct obj *);                           \
    memset(&new_env->env, 0, sizeof(S));                                       \
    (NAME) = (struct env_obj *)new_env;                                        \
  } while (0)

#define OBJECT_CLOSURE_ONE_NEW(NAME, FN, ENV)                                  \
  struct obj *(NAME);                                                          \
  do {                                                                         \
    struct closure_obj *new_obj = alloca(sizeof(struct closure_obj));          \
    *new_obj = object_closure_one_new((FN), (ENV));                            \
    TOUCH_OBJECT(new_obj, "closure_one_new");                                  \
    (NAME) = (struct obj *)new_obj;                                            \
  } while (0)

#define OBJECT_CLOSURE_TWO_NEW(NAME, FN, ENV)                                  \
  struct obj *(NAME);                                                          \
  do {                                                                         \
    struct closure_obj *new_obj = alloca(sizeof(struct closure_obj));          \
    *new_obj = object_closure_two_new((FN), (ENV));                            \
    TOUCH_OBJECT(new_obj, "closure_two_new");                                  \
    (NAME) = (struct obj *)new_obj;                                            \
  } while (0)

#define OBJECT_CELL_OBJ_NEW(NAME, VAL)                                         \
  struct obj *(NAME);                                                          \
  do {                                                                         \
    struct cell_obj *new_obj = alloca(sizeof(struct cell_obj));                \
    new_obj->base = object_base_new(OBJ_CELL);                                 \
    new_obj->val = (VAL);                                                      \
    TOUCH_OBJECT(new_obj, "object_cell_new");                                  \
    (NAME) = (struct obj *)new_obj;                                            \
  } while (0)

#define OBJECT_HT_OBJ_NEW(NAME)                 \
  struct

#ifdef DEBUG_TOUCH
#define TOUCH_OBJECT(OBJ, S)                                                   \
  do {                                                                         \
    fprintf(stderr,                                                            \
            "touching object %p tag: %d, last touched by %s: (%s:%d:%s)\n",    \
            (void *)(OBJ), ((struct obj *)(OBJ))->tag,                         \
            ((struct obj *)(OBJ))->last_touched_by, __func__, __LINE__, (S));  \
    ALLOC_SPRINTF(((struct obj *)(OBJ))->last_touched_by, "(%s:%d:%s)",        \
                  __func__, __LINE__, (S));                                    \
  } while (0)
#else
#define TOUCH_OBJECT(OBJ, S)                                                   \
  do {                                                                         \
  } while (0)
#endif // DEBUG_TOUCH

enum __attribute__((__packed__)) closure_size {
  CLOSURE_ONE = 0,
  CLOSURE_TWO,
};

enum __attribute__((__packed__)) zyn_object_tag {
  OBJ_CLOSURE = 1,
  ENV_OBJ,
  OBJ_INT,
  OBJ_STR,
  OBJ_CONS,
  OBJ_CELL,
  OBJ_HT,
  OBJ_BOOL,
};

#define LAST_OBJ_TYPE OBJ_HT

enum __attribute__((__packed__)) gc_mark_type { WHITE = 0, GREY, BLACK };

struct obj {
  enum zyn_object_tag tag;
  enum gc_mark_type mark;
  bool on_stack;
#ifdef DEBUG
  char *last_touched_by;
#endif
};

// builtin objects

struct env_obj {
  struct obj base;
  size_t len;
  struct obj *env[];
};

struct cell_obj {
  struct obj base;
  struct obj *val;
};

struct cons_obj {
  struct obj base;
  struct obj *car;
  struct obj *cdr;
};

struct closure_obj {
  struct obj base;
  enum closure_size size;
  union {
    void (*fn_1)(struct obj *, struct env_obj *);
    void (*fn_2)(struct obj *, struct obj *, struct env_obj *);
  };
  struct env_obj *env;
};

struct int_obj {
  struct obj base;
  int64_t val;
};

struct bool_obj {
  struct obj base;
  bool val;
};

struct string_obj {
  struct obj base;
  size_t len;
  const char buf[];
};


DEFINE_HASH(struct obj *, struct obj *, obj);

struct ht_obj {
  struct obj base;
  struct hash_table_obj *ht;
};

struct thunk {
  struct closure_obj *closr;
  union {
    struct {
      struct obj *rand;
    } one;
    struct {
      struct obj *rand;
      struct obj *cont;
    } two;
  };
};

void call_closure_one(struct obj *, struct obj *);
void call_closure_two(struct obj *, struct obj *, struct obj *);
void zayin_start(struct thunk *);
void run_minor_gc(struct thunk *);

struct obj object_base_new(enum zyn_object_tag);
struct closure_obj object_closure_one_new(void (*)(struct obj *,
                                                   struct env_obj *),
                                          struct env_obj *);
struct closure_obj object_closure_two_new(void (*)(struct obj *, struct obj *,
                                                   struct env_obj *),
                                          struct env_obj *);
struct int_obj object_int_obj_new(int64_t);
struct bool_obj object_bool_obj_new(bool);
struct cons_obj object_cons_obj_new(struct obj *, struct obj *);
struct ht_obj object_ht_obj_new(void);

bool eq_obj_impl(struct obj *, struct obj *);

void *stack_ptr(void);

// Add this to your base.h file:
#define OBJECT_CLOSURE_ONE_NEW_DIRECT(NAME, FN, ENV) \
  NAME.base.mark = gc_color_red; \
  NAME.base.tag = OBJ_CLOSURE; \
  NAME.size = CLOSURE_ONE; \
  NAME.fn_1 = (void (*)(struct obj *, struct env_obj *))FN; \
  NAME.env = ENV;


// NEW Implementation

#include <math.h>
#include <complex.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include <stdint.h>
#include <dlfcn.h>
#include "bignum.h"

#ifdef ZYN_HIGH_RES_TIMERS
/**
 * \defgroup hrt High resolution timers
 */
/**@{*/
long long hrt_get_current();
long long hrt_cmp_current(long long tstamp);
void hrt_log_delta(const char *label, long long tstamp);
/**@}*/
#endif

/**
 * Generic object type
 * \ingroup objects
 */
typedef void *object;

/**
 * Define a unique tag for each possible type of object.
 *
 * Remember to update tag_names in runtime.c when adding new tags
 *\ingroup objects
 */
enum object_tag {
  closure0_tag = 0, closure1_tag = 1, closureN_tag = 2, macro_tag = 3   // Keep closures here for quick type checking
  , boolean_tag = 4, bytevector_tag = 5, c_opaque_tag = 6, cond_var_tag =
      7, cvar_tag = 8, double_tag = 9, eof_tag = 10, forward_tag =
      11, integer_tag = 12, bignum_tag = 13, mutex_tag = 14, pair_tag =
      15, port_tag = 16, primitive_tag = 17, string_tag = 18, symbol_tag =
      19, vector_tag = 20, complex_num_tag = 21, atomic_tag = 22, void_tag =
      23, record_tag = 24
};

/**
 * Returns a true value if object is not a closure, or false otherwise
 */
#define obj_is_not_closure(obj) \
  ((obj == NULL) || is_value_type(obj) || (type_of(obj) > macro_tag))

/**
 * Defines the size of object tags
 * \ingroup objects
 */
typedef unsigned char tag_type;

/**
 * Access an object's tag.
 * \ingroup objects
 */
#define type_of(obj) (((pair_type *) obj)->tag)

/**
 * \defgroup gc Garbage collection
 *
 * @brief The Cyclone runtime's garbage collector (GC)
 *
 * When using the FFI there is normally no need to call
 * into this code unless something is specifically mentioned
 * in the User Manual.
 */
/**@{*/

/**
 * \defgroup gc_major Major GC
 * @brief Major GC is responsible for removing unused objects from
 * the heap.
 */
/**@{*/

////////////////////////////////
// Parameters for size of a "page" on the heap (the second generation GC), in bytes.

/** Grow first page by adding this amount to it */
#define GROW_HEAP_BY_SIZE (2 * 1024 * 1024)

/** Size of the first page */
#define INITIAL_HEAP_SIZE (3 * 1024 * 1024)

/** Normal size of a heap page */
#define HEAP_SIZE (8 * 1024 * 1024)

// End heap page size parameters
////////////////////////////////

/////////////////////////////
// Major GC tuning parameters

/** Start GC cycle if % heap space free below this percentage */
#define GC_COLLECTION_THRESHOLD 0.0125  //0.05

/** Start GC cycle if fewer than this many heap pages are unswept */
#define GC_COLLECT_UNDER_UNSWEPT_HEAP_COUNT 3

/** After major GC, grow the heap so at least this percentage is free */
#define GC_FREE_THRESHOLD 0.40
// END GC tuning
/////////////////////////////

/** Number of functions to save for printing call history */
#define MAX_STACK_TRACES 10

/** Show diagnostic information for the GC when program terminates */
#define DEBUG_SHOW_DIAG 0

/** Show diagnostic information before/after sweeping */
#define GC_DEBUG_SHOW_SWEEP_DIAG 0

/** GC debugging flag */
#define GC_DEBUG_TRACE 0

/** GC debugging flag */
#define GC_DEBUG_VERBOSE 0

/**
 *  Additional runtime checking of the GC system.
 *  This is here because these checks should not be
 *  necessary if GC is working correctly.
 */
#define GC_SAFETY_CHECKS 0

/** Generic constant used for GC sleep/wake */
#define NANOSECONDS_PER_MILLISECOND 1000000

/* GC data structures */

/**
 * Group heap pages by type, to attempt to limit fragmentation
 * and improve performance.

 TODO: starting to run into problems when adding additional "sizes" of heap page,
 possibly due to increasing amounts of page faults due to non-locality???

 Basically for X86_64 everything works great when a 96 byte heap is added, but slows way down when additional
 heaps (128, 160) are also added.

 32 bit x86 is starting to have trouble with just a 96 byte heap added.
 */

// Type starts at 0 and ends at LAST_FIXED_SIZE_HEAP_TYPE
// Presently each type contains buckets of a multiple of 32 bytes
// EG: 0 ==> 32
//     1 ==> 64, etc
typedef int gc_heap_type;

/** The first heap type that is not fixed-size */
#if INTPTR_MAX == INT64_MAX
#define LAST_FIXED_SIZE_HEAP_TYPE 2
#else
#define LAST_FIXED_SIZE_HEAP_TYPE 1
#endif

#define HEAP_REST (LAST_FIXED_SIZE_HEAP_TYPE + 1)
#define HEAP_HUGE (HEAP_REST + 1)

/** The number of `gc_heap_type`'s */
#define NUM_HEAP_TYPES (HEAP_HUGE + 1)

/**
 * Linked list of free memory chunks on a heap page
 */
typedef struct gc_free_list_t gc_free_list;
struct gc_free_list_t {
  unsigned int size;
  gc_free_list *next;
};

/**
 * Heap page
 *
 * @brief Contains data for a single page of the heap.
 *
 * Note there are groups of parameters to support:
 * - Bump-allocation - This type of allocation is faster but only applicable when a page is first created or empty.
 * - Lazy sweep
 */
typedef struct gc_heap_t gc_heap;
struct gc_heap_t {
  gc_heap_type type;
  /** Size of the heap page in bytes */
  unsigned int size;
  /** Keep empty page alive this many times before freeing */
  unsigned char ttl;
  /** Bump: Track remaining space; this is useful for bump&pop style allocation */
  unsigned int remaining;
  /** For fixed-size heaps, only allocate blocks of this size */
  unsigned block_size;
  /** Lazy-sweep: Amount of heap data that is free */
  unsigned int free_size;
  /** Lazy-sweep: Determine if the heap is full */
  unsigned char is_full;
  /** Lazy-sweep: Determine if the heap has been swept */
  unsigned char is_unswept;
  /** Lazy-sweep: Start GC cycle if fewer than this many heap pages are unswept */
  int num_unswept_children;
  /** Last size of object that was allocated, allows for optimizations */
  unsigned int last_alloc_size;
  /** Next page that has free space, lets alloc find that page faster */
  gc_heap *next_free;
  /** Linked list of free memory blocks in this page */
  gc_free_list *free_list;
  /** Next page in this heap */
  gc_heap *next;                // TBD, linked list is not very efficient, but easy to work with as a start
  /** Actual data in this page */
  char *data;
  /** End of the data when using bump alllocation or NULL when using free lists */
  char *data_end;
};

/**
 * A heap root is the heap's first page
 */
typedef struct gc_heap_root_t gc_heap_root;
struct gc_heap_root_t {
  gc_heap **heap;
};

/**
 * Header added to each object for GC purposes
 */
typedef struct gc_header_type_t gc_header_type;
struct gc_header_type_t {
  unsigned char mark;           // mark bits
  unsigned char grayed:1;       // stack object to be grayed when moved to heap
  unsigned char immutable:1;    // Flag normally mutable obj (EG: pair) as read-only
};

/** Get an object's `mark` value */
#define mark(x) (((list) x)->hdr.mark)

/** Get an object's `grayed` value */
#define grayed(x) (((list) x)->hdr.grayed)

//** Access an object's "immutable" field */
#define immutable(x) (((list) x)->hdr.immutable)

/** Enums for tri-color marking */
typedef enum { STATUS_ASYNC, STATUS_SYNC1, STATUS_SYNC2
} gc_status_type;

/** Stages of the Major GC's collector thread */
typedef enum { STAGE_CLEAR_OR_MARKING, STAGE_TRACING
      //, STAGE_REF_PROCESSING
  , STAGE_SWEEPING, STAGE_RESTING
} gc_stage_type;

// Constant colors are defined here.
// The mark/clear colors are defined in the gc module because
// the collector swaps their values as an optimization.

/** Memory not to be collected by major GC, such as on the stack */
#define gc_color_red  0

/** Unallocated memory */
#define gc_color_blue 2

/** Mark buffers */
typedef struct mark_buffer_t mark_buffer;
struct mark_buffer_t {
  void **buf;
  unsigned buf_len;
  mark_buffer *next;
};

/** Threading */
typedef enum { ZYN_THREAD_STATE_NEW, ZYN_THREAD_STATE_RUNNABLE,
  ZYN_THREAD_STATE_BLOCKED, ZYN_THREAD_STATE_BLOCKED_COOPERATING,
  ZYN_THREAD_STATE_TERMINATED
} cyc_thread_state_type;

/**
 * Thread data structures
 * @brief Each thread is given an instance of this struct to
 *        maintain its state
 */
typedef struct gc_thread_data_t gc_thread_data;
struct gc_thread_data_t {
  /** Call History: circular buffer of previous calls */
  char **stack_traces;
  /** Call History: Current place in the buffer */
  int stack_trace_idx;
  /** Call History: Previous frame written to call history; allows us to avoid duplicate entries */
  char *stack_prev_frame;
  /** Current state of this thread */
  cyc_thread_state_type thread_state;
  /** Minor GC: Data needed to initiate stack-based minor GC */
  char *stack_start;
  /** Minor GC: Data needed to initiate stack-based minor GC, defines the end of the memory range */
  char *stack_limit;
  /** Minor GC: write barrier */
  void **mutations;
  /** Minor GC: Size of the minor GC write barrier */
  int mutation_buflen;
  /** Minor GC: Number of entries in the minor GC write barrier */
  int mutation_count;
  /** Minor GC: Is minor collection of globals necessary? */
  unsigned char globals_changed;
  /** Minor GC: List of objects moved to heap during minor GC */
  void **moveBuf;
  /** Minor GC: Length of `moveBuf` */
  int moveBufLen;
  /** Heap GC: mark color used for new allocations */
  unsigned char gc_alloc_color;
  /** Heap GC: mark color the major GC is currently using tracing. This can be different than the alloc color due to lazy sweeping */
  unsigned char gc_trace_color;
  /** Heap GC: Is the major GC done tracing? */
  uint8_t gc_done_tracing;
  /** Heap GC: current state of the collector */
  int gc_status;
  /** Heap GC: index of last write to the mark buffer */
  int last_write;
  /** Heap GC: index of last read from the mark buffer */
  int last_read;
  /** Heap GC:
   *  Need this because minor GC may still be moving objects to the heap and
   *  if we try to trace before minor GC is done, some of the objects may be
   *  missed. So we "pend" them until minor GC is done and we know everything
   *  is on the heap.
   */
  int pending_writes;
  /** Heap GC: buffer of grey objects */
  mark_buffer *mark_buffer;
  /** Heap GC: length of the mark buffer */
  int mark_buffer_len;
  /** Heap GC: lock used to coordinate access between the collector and this thread */
  pthread_mutex_t lock;
  /** Id of the current thread */
  pthread_t thread_id;
  /** Heap GC: Root of this thread's heap */
  gc_heap_root *heap;
  /** Heap GC: Cached amount of free heap space, so we do not need to recalculate on the fly */
  uintptr_t *cached_heap_free_sizes;
  /** Heap GC: Cached total amount of heap space */
  uintptr_t *cached_heap_total_sizes;
  /** Heap GC: Number of "huge" allocations by this thread */
  int heap_num_huge_allocations;
  /** Heap GC: Keep track of number of minor GC's for use by the major GC */
  int num_minor_gcs;
  /** Exception handler stack */
  object exception_handler_stack;
  /** Parameter object data */
  object param_objs;
  /** Need the following to perform longjmp's */
  jmp_buf *jmp_start;
  /** After longjmp, pick up execution here */
  object gc_cont;
  /** After longjmp, pass continuation these arguments */
  object *gc_args;
  /** Length of `gc_args` */
  short gc_num_args;
  /**  Thread object, if applicable */
  object scm_thread_obj;
};

/* GC prototypes */
void gc_initialize(void);
void gc_add_new_unrunning_mutator(gc_thread_data * thd);
void gc_add_mutator(gc_thread_data * thd);
void gc_remove_mutator(gc_thread_data * thd);
int gc_is_mutator_active(gc_thread_data * thd);
int gc_is_mutator_new(gc_thread_data * thd);
void gc_sleep_ms(int ms);
gc_heap *gc_heap_create(int heap_type, size_t size, gc_thread_data * thd);
gc_heap *gc_heap_free(gc_heap * page, gc_heap * prev_page);
int gc_heap_merge(gc_heap * hdest, gc_heap * hsrc);
void gc_merge_all_heaps(gc_thread_data * dest, gc_thread_data * src);
void gc_print_stats(gc_heap * h);
gc_heap *gc_grow_heap(gc_heap * h, size_t size, gc_thread_data * thd);
char *gc_copy_obj(object hp, char *obj, gc_thread_data * thd);
void *gc_try_alloc(gc_heap * h, size_t size, char *obj, gc_thread_data * thd);
void *gc_try_alloc_slow(gc_heap * h_passed, gc_heap * h, size_t size, char *obj,
                        gc_thread_data * thd);
void *gc_alloc(gc_heap_root * h, size_t size, char *obj, gc_thread_data * thd,
               int *heap_grown);
void *gc_alloc_bignum(gc_thread_data * data);
size_t gc_allocated_bytes(object obj, gc_free_list * q, gc_free_list * r);
gc_heap *gc_heap_last(gc_heap * h);

void gc_heap_create_rest(gc_heap * h, gc_thread_data * thd);
void *gc_try_alloc_rest(gc_heap * h, size_t size, char *obj,
                        gc_thread_data * thd);
void *gc_alloc_rest(gc_heap_root * hrt, size_t size, char *obj,
                    gc_thread_data * thd, int *heap_grown);
void gc_init_fixed_size_free_list(gc_heap * h);

//size_t gc_heap_total_size(gc_heap * h);
//size_t gc_heap_total_free_size(gc_heap *h);
//size_t gc_collect(gc_heap *h, size_t *sum_freed);
//void gc_mark(gc_heap *h, object obj);
void gc_request_mark_globals(void);
void gc_mark_globals(object globals, object global_table);
//size_t gc_sweep(gc_heap * h, size_t * sum_freed_ptr, gc_thread_data *thd);
gc_heap *gc_sweep(gc_heap * h, gc_thread_data * thd);
void gc_thr_grow_move_buffer(gc_thread_data * d);
void gc_thread_data_init(gc_thread_data * thd, int mut_num, char *stack_base,
                         long stack_size);
void gc_thread_data_free(gc_thread_data * thd);
// Prototypes for mutator/collector:
/**
 * @brief Determine if object lives on the thread's stack
 * @param low_limit Temporary object at the current "end" of the stack
 * @param thd Mutator's thread data
 * @param obj Object to inspect
 * @return True if `obj` is on the mutator's stack, false otherwise
 */
#define gc_is_stack_obj(low_limit, thd, obj) \
 (stack_overflow(((object)low_limit), ((object)obj)) && \
  stack_overflow(((object)obj), ((object)((gc_thread_data *)thd)->stack_start)))
void gc_mut_update(gc_thread_data * thd, object old_obj, object value);
void gc_mut_cooperate(gc_thread_data * thd, int buf_len);
void gc_mark_gray(gc_thread_data * thd, object obj);
void gc_mark_gray2(gc_thread_data * thd, object obj);
void gc_collector_trace();
void gc_empty_collector_stack();
void gc_handshake(gc_status_type s);
void gc_post_handshake(gc_status_type s);
void gc_wait_handshake();
void gc_start_collector();
void gc_mutator_thread_blocked(gc_thread_data * thd, object cont);
void gc_mutator_thread_runnable(gc_thread_data * thd, object result,
                                object maybe_copied);
void zyn_make_shared_object(void *data, object k, object obj);
#define set_thread_blocked(d, c) \
  gc_mutator_thread_blocked(((gc_thread_data *)d), (c))
/**
 * @brief Return from a blocked thread
 */
#define return_thread_runnable(d, r) \
  gc_mutator_thread_runnable(((gc_thread_data *)d), (r), NULL)
/**
 * @brief Return from a blocked thread with an object that may have been copied.
 *        If the object was copied we need to check and may need to copy it again.
 */
#define return_thread_runnable_with_obj(d, r, maybe_copied) \
  gc_mutator_thread_runnable(((gc_thread_data *)d), (r), maybe_copied)
/*
//#define do_with_blocked_thread(data, cont, result, body) \
//  set_thread_blocked((data), (cont)); \
//  body \
//  return_thread_runnable((data), (result));
*/

/**@}*/

/**
 * \defgroup gc_minor Minor GC
 * @brief Minor GC is called periodically to copy live objects off of a thread stack
 *
 */
/**@{*/

/**
 * Maximum number of args that GC will accept
 */
#define NUM_GC_ARGS 128

/**
 * Which way does the CPU grow its stack?
 */
#define STACK_GROWTH_IS_DOWNWARD 1

/**
 * Size of the stack buffer, in bytes.
 * This is used as the first generation of the GC.
 */
#define STACK_SIZE 500000

/**
 * Do not allocate objects larger than this on the stack.
 */
#define MAX_STACK_OBJ (STACK_SIZE * 2)

/** Determine if stack has overflowed */
#if STACK_GROWTH_IS_DOWNWARD
#define stack_overflow(x,y) ((x) < (y))
#else
#define stack_overflow(x,y) ((x) > (y))
#endif

/**
 * Access an object's forwarding pointer.
 * Note this is only applicable when objects are relocated
 * during minor GC.
 * \ingroup objects
 */
#define forward(obj) (((pair_type *) obj)->pair_car)

/**
 * \defgroup gc_minor_mut Mutation table
 * @brief Mutation table to support the minor GC write barrier
 */
/**@{*/
void add_mutation(void *data, object var, int index, object value);
void clear_mutations(void *data);
/**@}*/

/**
 * \defgroup gc_minor_sh_obj Shared object write barrier
 * @brief Minor GC write barrier to ensure there are no references to stack objects from the heap.
 */
/**@{*/
object transport_stack_value(gc_thread_data * data, object var, object value,
                             int *run_gc);
/**@}*/

/**@}*/

// END GC section
/**@}*/

/**
 * \defgroup ffi Foreign Function Interface
 */
/**@{*/
object zyn_scm_call(gc_thread_data * parent_thd, object fnc, int argc,
                    object * args);
object zyn_scm_call_no_gc(gc_thread_data * parent_thd, object fnc, object arg);
/**@}*/

/**
 * \defgroup datatypes Data types
 * @brief All of the Scheme data types provided by Cyclone.
 */
/**@{*/

/**
 * \defgroup immediates Immediate objects
 *
 *  @brief Objects that do not require memory allocation.
 *
 *  Immediate objects (also known as value types) are stored directly within
 *  the bits that would otherwise be a pointer to an object type. Since
 *  all of the data is contained in those bits, a value type is never
 *  allocated on the heap and never needs to be garbage collected,
 *  making them very efficient.
 *
 *  Depending on the underlying architecture, compiler, etc these types
 *  have extra least significant bits that can be used to mark them as
 *  values instead of objects (IE, pointer to a tagged object).
 *  On many machines, addresses are multiples of four, leaving the two
 *  least significant bits free - from lisp in small pieces.
 *
 *  The possible types are:
 *
 *  - 0x00 - pointer (an object type)
 *  - 0x01 - integer (also known as fixnum)
 *  - 0x10 - char
 */
/**@{*/

/** Maximum allowed value of a fixnum */
#define ZYN_FIXNUM_MAX 1073741823

/** Minimum allowed value of a fixnum */
#define ZYN_FIXNUM_MIN -1073741824

/**
 * Explicit character type now that we are using UTF-8.
 * Chars are still value types though
 */
typedef uint32_t char_type;

/**
 * Determine if an object is an integer.
 */
#define obj_is_int(x)  ((unsigned long)(x) & (unsigned long)1)

/**
 * Convert from an object to an integer.
 */
//#define obj_obj2int(n)   (((long)((ulong)(n) & ~1))/(long)(1uL<<1))
#define obj_obj2int(x) ((long)((uintptr_t)x)>>1)

/**
 * Convert from an integer to an object.
 */
//#define obj_int2obj(n) ((void *) ((((long)(n))*(long)(1uL<<1)) | 1))
#define obj_int2obj(c) ((void *)((((long)c)*2) | 1))

/**
 * Determine if the object is a char.
 */
#define obj_is_char(x)  (((unsigned long)(x) & (unsigned long)3) == 2)

/**
 * Convert from an object to a char.
 */
#define obj_obj2char(x) (char_type)((uintptr_t)(x)>>2)

/**
 * Convert from a char to an object.
 */
#define obj_char2obj(c) ((void *)((((uintptr_t)c)<<2) | 2))

/**
 * Is the given object a value type?
 */
#define is_value_type(x) ((unsigned long)(x) & (unsigned long)3)

/**
 * Is the given object an object (non-immediate) type?
 */
#define is_object_type(x) ((x != NULL) && !is_value_type(x))

/**@}*/

/**
 * \defgroup objects Objects
 * @brief Definitions and code for memory-allocated objects.
 *
 * Most Scheme data types are defined as object types.
 *
 * Each object type contains a header for garbage collection and a
 * tag that identifies the type of object, as well as any object-specific
 * fields.
 *
 * Most object types are allocated on the nursery (the C stack) and
 * relocated to the garbage-collected heap during minor GC. It is only
 * safe for an object on the nursery to be used by the thread that
 * created it, as that object could be relocated at any time.
 */
/**@{*/

/** Function type */
typedef void (*function_type)(void *data, object clo, int argc, object * args);

/** Non-CPS function type */
typedef object(*inline_function_type) ();

/**
 * @brief C-variable integration type - wrapper around a Cyclone object pointer
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  /** Variable pointer. Note GC assumes this is a Cyclone object! */
  object *pvar;
} cvar_type;
typedef cvar_type *cvar;

/**
 * Create a new cvar in the nursery
 */
#define make_cvar(n,v) \
  cvar_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.hdr.immutable = 0; \
  n.tag = cvar_tag; \
  n.pvar = v;

/**
 * @brief C Opaque type - a wrapper around a pointer of any type.
 *
 * Note this requires application code to free any memory
 * before an object is collected by GC.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  unsigned char collect_ptr;
  /** This pointer can be anything, GC will not collect it
      unless collect_ptr is set */
  void *ptr;
} c_opaque_type;
typedef c_opaque_type *c_opaque;

/** Create a new opaque in the nursery */
#define make_c_opaque(var, p) \
  c_opaque_type var; \
  var.hdr.mark = gc_color_red; \
  var.hdr.grayed = 0; \
  var.hdr.immutable = 0; \
  var.tag = c_opaque_tag; \
  var.collect_ptr = 0; \
  var.ptr = p;

/** Access the Opaque's pointer */
#define opaque_ptr(x) (((c_opaque)x)->ptr)

/** Access the Opaque's "collect pointer" field */
#define opaque_collect_ptr(x) (((c_opaque)x)->collect_ptr)

/**
 * @brief The mutex thread synchronization type
 *
 * Mutexes are always allocated directly on the heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  pthread_mutex_t lock;
} mutex_type;
typedef mutex_type *mutex;

/**
 * @brief The condition variable thread synchronization type
 *
 * Condition variables are always allocated directly on the heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  pthread_cond_t cond;
} cond_var_type;
typedef cond_var_type *cond_var;

/**
 * @brief The atomic thread synchronization type
 *
 * Atomics are always allocated directly on the heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object obj;
} atomic_type;
typedef atomic_type *atomic;

/**
 * @brief The boolean type: True or False
 *
 * Booleans always refer to one of the objects `boolean_t` or `boolean_f`
 * which are created by the runtime.
 */
typedef struct {
  gc_header_type hdr;
  const tag_type tag;
  const char *desc;
} boolean_type;
typedef boolean_type *boolean;

#define boolean_desc(x) (((boolean_type *) x)->desc)

#define make_boolean(x) (x ? boolean_t : boolean_f)

/**
 * @brief Symbols are similar to strings, but only one instance of each
 * unique symbol is created, so comparisons are O(1).
 *
 * A thread-safe symbol table is used at runtime to store all of
 * the program's symbols.
 */
typedef struct {
  gc_header_type hdr;
  const tag_type tag;
  const char *desc;
} symbol_type;
typedef symbol_type *symbol;

#define symbol_desc(x) (((symbol_type *) x)->desc)

#define defsymbol(name) \
static object quote_##name = NULL;

/* Define numeric types */

/**
 * @brief Deprecated - boxed integers
 *
 * The integer object type is deprecated, integers should be stored using value types instead.
 * This is only still here because it is used internally by the runtime.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int value;
  int padding;                  // Prevent mem corruption if sizeof(int) < sizeof(ptr)
} integer_type;

/**
 * @brief Exact integer of unlimited precision.
 *
 * The backing store is the `mp_int` data type from LibTomMath.
 *
 * Note memory for `mp_int` is allocated via `malloc`, so bignums must
 * always be allocated on Cyclone's heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  mp_int bn;
} bignum_type;

/** Allocate a new bignum on the heap */
#define alloc_bignum(data, p) \
  bignum_type *p = gc_alloc_bignum((gc_thread_data *)data);

/** Helper for handling return value of a bignum function call */
#define BIGNUM_CALL(x) { \
  int __bn_mp_rv; \
  if ((__bn_mp_rv = (x)) != MP_OKAY) { \
    fprintf(stderr, "Error calling bignum function: %s\n", \
      mp_error_to_string(__bn_mp_rv)); \
    exit(1); \
  } \
}

/**
 * @brief Complex number
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  double complex value;
} complex_num_type;

/** Create a new complex number in the nursery */
#define make_complex_num(n,r,i) \
  complex_num_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.tag = complex_num_tag; \
  n.value = (r + (i * I));

#define alloca_complex_num(n,r,i) \
  complex_num_type *n = alloca(sizeof(complex_num_type)); \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->tag = complex_num_tag; \
  n->value = (r + (i * I));

/** Assign given complex value to the given complex number object pointer */
#define assign_complex_num(pobj,v) \
  ((complex_num_type *)pobj)->hdr.mark = gc_color_red; \
  ((complex_num_type *)pobj)->hdr.grayed = 0; \
  ((complex_num_type *)pobj)->tag = complex_num_tag; \
  complex_num_value(pobj) = v;

/**
 * @brief Double-precision floating point type, also known as a flonum.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  double value;
} double_type;

/** Create a new double in the nursery */
#define make_double(n,v) \
  double_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.tag = double_tag; \
  n.value = v;

/** Create a new double in the nursery using alloca */
#define alloca_double(n,v) \
  double_type *n = alloca(sizeof(double_type)); \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->tag = double_tag; \
  n->value = v;

/** Assign given double value to the given double object pointer */
#define assign_double(pobj,v) \
  ((double_type *)pobj)->hdr.mark = gc_color_red; \
  ((double_type *)pobj)->hdr.grayed = 0; \
  ((double_type *)pobj)->tag = double_tag; \
  double_value(pobj) = v;

/** Access the integer_type integer value directly */
#define integer_value(x) (((integer_type *) x)->value)

/** Access the double directly */
#define double_value(x) (((double_type *) x)->value)

/** Access a bignum's `mp_int` directly */
#define bignum_value(x) (((bignum_type *) x)->bn)

/** Access the complex number directly */
#define complex_num_value(x) (((complex_num_type *) x)->value)

/**
 * This enumeration complements the comparison types from LibTomMath,
 * and provides constants for each of the comparison operators.
 */
typedef enum {
  ZYN_BN_LTE = -2, ZYN_BN_LT = MP_LT, ZYN_BN_EQ = MP_EQ, ZYN_BN_GT =
      MP_GT, ZYN_BN_GTE = 2
} bn_cmp_type;

/**
 * @brief The string type
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int num_cp;
  int len;
  char *str;
} string_type;

// TODO: below macros are obsolete, need new ones that populate num_cp and
// raise an error if an invalid UTF-8 char is detected

/** Create a new string in the nursery */
#define make_string(cs, s) string_type cs; \
{ int len = strlen(s); \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; \
  cs.num_cp = len; \
  cs.len = len; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len + 1);}

/**
 * Create a new string with the given length
 * (so it does not need to be computed)
 */
#define make_string_with_len(cs, s, length) string_type cs;  \
{ int len = length; \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = len; \
  cs.num_cp = len; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len); \
  cs.str[len] = '\0';}

/**
 * Create a string object using the given C string and length.
 * No allocation is done for the given C string.
 */
#define make_string_noalloc(cs, s, length) string_type cs; \
{ cs.hdr.mark = gc_color_red; cs.hdr.grayed = 0; cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = length; \
  cs.num_cp = length; \
  cs.str = s; }

/** Create a new string in the nursery */
#define make_utf8_string(data, cs, s) string_type cs; \
{ int len = strlen(s); \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; \
  cs.num_cp = zyn_utf8_count_code_points((uint8_t *)s); \
  if (cs.num_cp < 0) { \
    zyn_rt_raise_msg(data, "Invalid UTF-8 characters in string"); \
  } \
  cs.len = len; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len + 1);}

/**
 * Create a new string with the given length
 * (so it does not need to be computed)
 */
#define make_utf8_string_with_len(cs, s, length, num_code_points) string_type cs;  \
{ int len = length; \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = len; \
  cs.num_cp = num_code_points; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len); \
  cs.str[len] = '\0';}

/**
 * Create a string object using the given C string and length.
 * No allocation is done for the given C string.
 */
#define make_utf8_string_noalloc(cs, s, length) string_type cs; \
{ cs.hdr.mark = gc_color_red; cs.hdr.grayed = 0; cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = length; \
  cs.num_cp = length; \
  cs.str = s; }

/**
 * Allocate a new string, either on the stack or heap depending upon size
 */
#define alloc_string(_data, _s, _len, _num_cp) \
  if (_len >= MAX_STACK_OBJ) { \
    int heap_grown; \
    _s = gc_alloc(((gc_thread_data *)data)->heap,  \
                 sizeof(string_type) + _len + 1, \
                 boolean_f, /* OK to populate manually over here */ \
                 (gc_thread_data *)data,  \
                 &heap_grown); \
    ((string_type *) _s)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color; \
    ((string_type *) _s)->hdr.grayed = 0; \
    ((string_type *) _s)->hdr.immutable = 0; \
    ((string_type *) _s)->tag = string_tag; \
    ((string_type *) _s)->len = _len; \
    ((string_type *) _s)->num_cp = _num_cp; \
    ((string_type *) _s)->str = (((char *)_s) + sizeof(string_type)); \
  } else { \
    _s = alloca(sizeof(string_type)); \
    ((string_type *)_s)->hdr.mark = gc_color_red;  \
    ((string_type *)_s)->hdr.grayed = 0; \
    ((string_type *)_s)->hdr.immutable = 0; \
    ((string_type *)_s)->tag = string_tag;  \
    ((string_type *)_s)->len = _len; \
    ((string_type *)_s)->num_cp = _num_cp; \
    ((string_type *)_s)->str = alloca(sizeof(char) * (_len + 1)); \
  }

/**
 * Allocate a new bytevector, either on the stack or heap depending upon size
 */
#define alloc_bytevector(_data, _bv, _len) \
  if (_len >= MAX_STACK_OBJ) { \
    int heap_grown; \
    _bv = gc_alloc(((gc_thread_data *)data)->heap, \
                  sizeof(bytevector_type) + _len, \
                  boolean_f, /* OK to populate manually over here */ \
                  (gc_thread_data *)data, \
                  &heap_grown); \
    ((bytevector) _bv)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color; \
    ((bytevector) _bv)->hdr.grayed = 0; \
    ((bytevector) _bv)->hdr.immutable = 0; \
    ((bytevector) _bv)->tag = bytevector_tag; \
    ((bytevector) _bv)->len = _len; \
    ((bytevector) _bv)->data = (char *)(((char *)_bv) + sizeof(bytevector_type)); \
  } else { \
    _bv = alloca(sizeof(bytevector_type)); \
    ((bytevector) _bv)->hdr.mark = gc_color_red; \
    ((bytevector) _bv)->hdr.grayed = 0; \
    ((bytevector) _bv)->hdr.immutable = 0; \
    ((bytevector) _bv)->tag = bytevector_tag; \
    ((bytevector) _bv)->len = _len; \
    ((bytevector) _bv)->data = alloca(sizeof(char) * _len); \
  }

/** Get the length of a string, in characters (code points) */
#define string_num_cp(x) (((string_type *) x)->num_cp)

/** Get the length of a string, in bytes */
#define string_len(x) (((string_type *) x)->len)

/** Get a string object's C string */
#define string_str(x) (((string_type *) x)->str)

/* I/O types */

// TODO: FILE* may not be good enough
//       consider http://stackoverflow.com/questions/6206893/how-to-implement-char-ready-in-c
// TODO: a simple wrapper around FILE may not be good enough long-term
// TODO: how exactly mode will be used. need to know r/w, bin/txt

/**
 * @brief The port object type
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  void *unused;                 // Protect against forwarding pointer, ideally would not be needed.
  FILE *fp;
  int mode;
  unsigned char flags;
  unsigned int line_num;
  unsigned int col_num;
  unsigned int buf_idx;
  unsigned int tok_start;       // Start of token in mem_buf (end is unknown yet)
  unsigned int tok_end;         // End of token in tok_buf (start is tok_buf[0])
  char *tok_buf;                // Alternative buffer for tokens
  size_t tok_buf_len;
  char *mem_buf;
  size_t mem_buf_len;
  unsigned short read_len;
  char *str_bv_in_mem_buf;
  size_t str_bv_in_mem_buf_len;
} port_type;

#define ZYN_BINARY_PORT_FLAG 0x10

#define ZYN_IO_BUF_LEN 1024

/** Create a new port object in the nursery */
#define make_port(p,f,m) \
  port_type p; \
  p.hdr.mark = gc_color_red; \
  p.hdr.grayed = 0; \
  p.hdr.immutable = 0; \
  p.tag = port_tag; \
  p.fp = f; \
  p.mode = m; \
  p.flags = 0; \
  p.line_num = 1; \
  p.col_num = 1; \
  p.buf_idx = 0; \
  p.tok_start = 0; \
  p.tok_end = 0; \
  p.tok_buf = NULL; \
  p.tok_buf_len = 0; \
  p.mem_buf = NULL; \
  p.mem_buf_len = 0; \
  p.str_bv_in_mem_buf = NULL; \
  p.str_bv_in_mem_buf_len = 0; \
  p.read_len = 1;

/** Create a new input port object in the nursery */
#define make_input_port(p,f,rl) \
  port_type p; \
  p.hdr.mark = gc_color_red; \
  p.hdr.grayed = 0; \
  p.hdr.immutable = 0; \
  p.tag = port_tag; \
  p.fp = f; \
  p.mode = 1; \
  p.flags = 1; \
  p.line_num = 1; \
  p.col_num = 1; \
  p.buf_idx = 0; \
  p.tok_start = 0; \
  p.tok_end = 0; \
  p.tok_buf = malloc(ZYN_IO_BUF_LEN); \
  p.tok_buf_len = ZYN_IO_BUF_LEN; \
  p.mem_buf = malloc(ZYN_IO_BUF_LEN); \
  p.mem_buf_len = 0; \
  p.str_bv_in_mem_buf = NULL; \
  p.str_bv_in_mem_buf_len = 0; \
  p.read_len = rl;

/**
 * @brief Vector type
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int num_elements;
  object *elements;
} vector_type;
typedef vector_type *vector;

typedef struct {
  vector_type v;
  object arr[2];
} vector_2_type;
typedef struct {
  vector_type v;
  object arr[3];
} vector_3_type;
typedef struct {
  vector_type v;
  object arr[4];
} vector_4_type;
typedef struct {
  vector_type v;
  object arr[5];
} vector_5_type;

/** Create a new vector in the nursery */
#define make_empty_vector(v) \
  vector_type v; \
  v.hdr.mark = gc_color_red; \
  v.hdr.grayed = 0; \
  v.hdr.immutable = 0; \
  v.tag = vector_tag; \
  v.num_elements = 0; \
  v.elements = NULL;

/** Create an empty vector in the nursery using alloca */
#define alloca_empty_vector(v) \
  vector_type *v = alloca(sizeof(vector_type)); \
  v->hdr.mark = gc_color_red; \
  v->hdr.grayed = 0; \
  v->hdr.immutable = 0; \
  v->tag = vector_tag; \
  v->num_elements = 0; \
  v->elements = NULL;

/**
 * @brief Bytevector type
 *
 * Bytevectors are similar to regular vectors, but instead of containing
 * objects, each bytevector member is a 8-bit integer.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int len;
  char *data;
} bytevector_type;
typedef bytevector_type *bytevector;

/** Create a new bytevector in the nursery */
#define make_empty_bytevector(v) \
  bytevector_type v; \
  v.hdr.mark = gc_color_red; \
  v.hdr.grayed = 0; \
  v.hdr.immutable = 0; \
  v.tag = bytevector_tag; \
  v.len = 0; \
  v.data = NULL;

/** Create an empty bytevector in the nursery using alloca */
#define alloca_empty_bytevector(v) \
  bytevector_type *v = alloca(sizeof(bytevector_type)); \
  v->hdr.mark = gc_color_red; \
  v->hdr.grayed = 0; \
  v->hdr.immutable = 0; \
  v->tag = bytevector_tag; \
  v->len = 0; \
  v->data = NULL;

/**
 * @brief The pair (cons) type.
 *
 * Contrary to popular belief, Scheme does not actually have a list type.
 *
 * Instead there is a pair object composed two objects, the `car` and `cdr`.
 * A list can be created by storing values in the `car` and a pointer to
 * the rest of the list in `cdr`. A `NULL` in the `cdr` indicates the end
 * of a list.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object pair_car;
  object pair_cdr;
} pair_type;
typedef pair_type *list;
typedef pair_type *pair;

/** Create a new pair in the nursery */
#define make_pair(n,a,d) \
  pair_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.hdr.immutable = 0; \
  n.tag = pair_tag; \
  n.pair_car = a; \
  n.pair_cdr = d;

/** Create a new pair in the nursery using alloca */
#define alloca_pair(n,a,d) \
  pair_type *n = alloca(sizeof(pair_type)); \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->hdr.immutable = 0; \
  n->tag = pair_tag; \
  n->pair_car = a; \
  n->pair_cdr = d;

/** Create a new pair in the thread's heap */
void *gc_alloc_pair(gc_thread_data * data, object head, object tail);

/**
 * Set members of the given pair
 * @param n - Pointer to a pair object
 * @param a - Object to assign to car
 * @param d - Object to assign to cdr
 */
#define set_pair(n,a,d) \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->hdr.immutable = 0; \
  n->tag = pair_tag; \
  n->pair_car = a; \
  n->pair_cdr = d;

/**
 * Set members of the given pair, using a single expression
 * @param n - Pointer to a pair object
 * @param a - Object to assign to car
 * @param d - Object to assign to cdr
 */
#define set_pair_as_expr(n,a,d) \
 (((pair)(n))->hdr.mark = gc_color_red, \
  ((pair)(n))->hdr.grayed = 0, \
  ((pair)(n))->hdr.immutable = 0, \
  ((pair)(n))->tag = pair_tag, \
  ((pair)(n))->pair_car = a, \
  ((pair)(n))->pair_cdr = d, \
  (n))

//typedef list_1_type pair_type;
typedef struct {
  pair_type a;
  pair_type b;
} list_2_type;
typedef struct {
  pair_type a;
  pair_type b;
  pair_type c;
} list_3_type;
typedef struct {
  pair_type a;
  pair_type b;
  pair_type c;
  pair_type d;
} list_4_type;

/**
 * Create a pair with a single value.
 * This is useful to create an object that can be modified.
 */
#define make_cell(n,a) make_pair(n,a,NULL)
#define alloca_cell(n,a) alloca_pair(n,a,NULL)
#define set_cell_as_expr(n,a) set_pair_as_expr(n,a,NULL)

/**
 * \defgroup objects_unsafe_cxr Unsafe pair access macros
 * @brief Macros for fast - but unsafe - pair access
 *
 */
/**@{*/
/** Unsafely access a pair's `car` */
#define car(x)    (((pair_type *) x)->pair_car)
/** Unsafely access a pair's `cdr` */
#define cdr(x)    (((pair_type *) x)->pair_cdr)
#define caar(x)   (car(car(x)))
#define cadr(x)   (car(cdr(x)))
#define cdar(x)   (cdr(car(x)))
#define cddr(x)   (cdr(cdr(x)))
#define caaar(x)  (car(car(car(x))))
#define caadr(x)  (car(car(cdr(x))))
#define cadar(x)  (car(cdr(car(x))))
#define caddr(x)  (car(cdr(cdr(x))))
#define cdaar(x)  (cdr(car(car(x))))
#define cdadr(x)  (cdr(car(cdr(x))))
#define cddar(x)  (cdr(cdr(car(x))))
#define cdddr(x)  (cdr(cdr(cdr(x))))
#define caaaar(x) (car(car(car(car(x)))))
#define caaadr(x) (car(car(car(cdr(x)))))
#define caadar(x) (car(car(cdr(car(x)))))
#define caaddr(x) (car(car(cdr(cdr(x)))))
#define cadaar(x) (car(cdr(car(car(x)))))
#define cadadr(x) (car(cdr(car(cdr(x)))))
#define caddar(x) (car(cdr(cdr(car(x)))))
#define cadddr(x) (car(cdr(cdr(cdr(x)))))
#define cdaaar(x) (cdr(car(car(car(x)))))
#define cdaadr(x) (cdr(car(car(cdr(x)))))
#define cdadar(x) (cdr(car(cdr(car(x)))))
#define cdaddr(x) (cdr(car(cdr(cdr(x)))))
#define cddaar(x) (cdr(cdr(car(car(x)))))
#define cddadr(x) (cdr(cdr(car(cdr(x)))))
#define cdddar(x) (cdr(cdr(cdr(car(x)))))
#define cddddr(x) (cdr(cdr(cdr(cdr(x)))))
/**@}*/

/**
 * \defgroup objects_safe_cxr Safe pair access macros
 * @brief Macros for safe pair access
 *
 */
/**@{*/
#define zyn_caar(d, x) (zyn_car(d, zyn_car(d, x)))
#define zyn_cadr(d, x) (zyn_car(d, zyn_cdr(d, x)))
#define zyn_cdar(d, x) (zyn_cdr(d, zyn_car(d, x)))
#define zyn_cddr(d, x) (zyn_cdr(d, zyn_cdr(d, x)))
#define zyn_caaar(d, x) (zyn_car(d, zyn_car(d, zyn_car(d, x))))
#define zyn_caadr(d, x) (zyn_car(d, zyn_car(d, zyn_cdr(d, x))))
#define zyn_cadar(d, x) (zyn_car(d, zyn_cdr(d, zyn_car(d, x))))
#define zyn_caddr(d, x) (zyn_car(d, zyn_cdr(d, zyn_cdr(d, x))))
#define zyn_cdaar(d, x) (zyn_cdr(d, zyn_car(d, zyn_car(d, x))))
#define zyn_cdadr(d, x) (zyn_cdr(d, zyn_car(d, zyn_cdr(d, x))))
#define zyn_cddar(d, x) (zyn_cdr(d, zyn_cdr(d, zyn_car(d, x))))
#define zyn_cdddr(d, x) (zyn_cdr(d, zyn_cdr(d, zyn_cdr(d, x))))
#define zyn_caaaar(d, x) (zyn_car(d, zyn_car(d, zyn_car(d, zyn_car(d, x)))))
#define zyn_caaadr(d, x) (zyn_car(d, zyn_car(d, zyn_car(d, zyn_cdr(d, x)))))
#define zyn_caadar(d, x) (zyn_car(d, zyn_car(d, zyn_cdr(d, zyn_car(d, x)))))
#define zyn_caaddr(d, x) (zyn_car(d, zyn_car(d, zyn_cdr(d, zyn_cdr(d, x)))))
#define zyn_cadaar(d, x) (zyn_car(d, zyn_cdr(d, zyn_car(d, zyn_car(d, x)))))
#define zyn_cadadr(d, x) (zyn_car(d, zyn_cdr(d, zyn_car(d, zyn_cdr(d, x)))))
#define zyn_caddar(d, x) (zyn_car(d, zyn_cdr(d, zyn_cdr(d, zyn_car(d, x)))))
#define zyn_cadddr(d, x) (zyn_car(d, zyn_cdr(d, zyn_cdr(d, zyn_cdr(d, x)))))
#define zyn_cdaaar(d, x) (zyn_cdr(d, zyn_car(d, zyn_car(d, zyn_car(d, x)))))
#define zyn_cdaadr(d, x) (zyn_cdr(d, zyn_car(d, zyn_car(d, zyn_cdr(d, x)))))
#define zyn_cdadar(d, x) (zyn_cdr(d, zyn_car(d, zyn_cdr(d, zyn_car(d, x)))))
#define zyn_cdaddr(d, x) (zyn_cdr(d, zyn_car(d, zyn_cdr(d, zyn_cdr(d, x)))))
#define zyn_cddaar(d, x) (zyn_cdr(d, zyn_cdr(d, zyn_car(d, zyn_car(d, x)))))
#define zyn_cddadr(d, x) (zyn_cdr(d, zyn_cdr(d, zyn_car(d, zyn_cdr(d, x)))))
#define zyn_cdddar(d, x) (zyn_cdr(d, zyn_cdr(d, zyn_cdr(d, zyn_car(d, x)))))
#define zyn_cddddr(d, x) (zyn_cdr(d, zyn_cdr(d, zyn_cdr(d, zyn_cdr(d, x)))))
/**@}*/

/* Closure types */

/** @brief Closure for a macro */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
} macro_type;

/** @brief A closed-over function with no variables */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
} closure0_type;
/** @brief A closed-over function with one variable */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
  object element;
} closure1_type;
/** @brief A closed-over function with zero or more closed-over variables */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
  int num_elements;
  object *elements;
} closureN_type;

typedef closure0_type *closure0;
typedef closure1_type *closure1;
typedef closureN_type *closureN;
typedef closure0_type *closure;
typedef closure0_type *macro;

#define mmacro(c,f) \
  macro_type c; \
  c.hdr.mark = gc_color_red; \
  c.hdr.grayed = 0; \
  c.tag = macro_tag; \
  c.fn = f; \
  c.num_args = -1;

/**
 * Create a closure0 object
 * These objects are special and can be statically allocated as an optimization
 */
#define mclosure0(c, f) \
 static closure0_type c = { .hdr.mark = gc_color_red, .hdr.grayed = 0, .tag = closure0_tag, .fn = f, .num_args = -1 };  /* TODO: need a new macro that initializes num_args */

#define maclosure0(c,f,na) \
  closure0_type c; \
  c.hdr.mark = gc_color_red; \
  c.hdr.grayed = 0; \
  c.tag = closure0_tag; \
  c.fn = f; \
  c.num_args = na;

/**
 * Create a closure1 object in the nursery
 */
#define mclosure1(c,f,a) \
  closure1_type c; \
  c.hdr.mark = gc_color_red; \
  c.hdr.grayed = 0; \
  c.tag = closure1_tag; \
  c.fn = f; \
  c.num_args = -1; \
  c.element = a;

/**
 * @brief A function built into the runtime.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  const char *desc;
} primitive_type;
typedef primitive_type *primitive;

#define defprimitive(name, desc, fnc) \
static primitive_type name##_primitive = {primitive_tag, #desc, fnc}; \
static const object primitive_##name = &name##_primitive

/** Is x a primitive object? */
#define prim(x) (x && ((primitive)x)->tag == primitive_tag)

/** Return description of primitive object x */
#define prim_name(x) (((primitive_type *) x)->desc)

/**
 * @brief A union of all the constant-size objects.
 *
 * This type is used internally to (for example) pass a pointer
 * to an inline function that might need to use it for an allocation.
 */
typedef union {
  boolean_type boolean_t;
  pair_type pair_t;
  symbol_type symbol_t;
  primitive_type primitive_t;
  integer_type integer_t;
  double_type double_t;
  bignum_type bignum_t;
  complex_num_type complex_num_t;
} common_type;

#define return_copy(ptr, o) \
{ \
  tag_type t; \
  object obj = o; \
  if (!is_object_type(obj)) \
    return obj; \
  t = type_of(obj); \
  if (t == double_tag) { \
    ((common_type *)ptr)->double_t.hdr.mark = gc_color_red; \
    ((common_type *)ptr)->double_t.hdr.grayed = 0; \
    ((common_type *)ptr)->double_t.tag = double_tag; \
    ((common_type *)ptr)->double_t.value = double_value(obj); \
    return ptr; \
  } else { \
    return obj; \
  } \
}

/**@}*/
/**@}*/

typedef struct vpbuffer_t vpbuffer;
struct vpbuffer_t {
  void **buf;
  int len;
  int count;
};

vpbuffer *vp_create(void);
void vp_add(vpbuffer * v, void *obj);

/* Utility functions */
void **vpbuffer_realloc(void **buf, int *len);
void **vpbuffer_add(void **buf, int *len, int i, void *obj);
void vpbuffer_free(void **buf);

/* Bignum utility functions */
int zyn_bignum_cmp(bn_cmp_type type, object x, int tx, object y, int ty);
void zyn_int2bignum(int n, mp_int * bn);

/* Remaining GC prototypes that require objects to be defined */
void *gc_alloc_from_bignum(gc_thread_data * data, bignum_type * src);

/**
 * Do a minor GC
 * \ingroup gc_minor
 */
int gc_minor(void *data, object low_limit, object high_limit, closure cont,
             object * args, int num_args);

void zyn_import_shared_object(void *data, object cont, object filename,
                              object entry_pt_fnc);

/**
 * The boolean True value.
 * \ingroup objects
 */
extern const object boolean_t;
/**
 * The boolean False value.
 * \ingroup objects
 */
extern const object boolean_f;
/**
 * The void value.
 * \ingroup objects
 */
extern const object quote_void;
/**
 * The EOF value.
 * \ingroup objects
 */
extern const object zyn_EOF;

/**
 * The void value.
 * \ingroup objects
 */
extern const object zyn_VOID;

/**
 * The record marker value.
 * \ingroup objects
 */
extern const object zyn_RECORD_MARKER;

/**
 * \ingroup gc_minor
 */
void GC(void *, closure, object *, int);

/**
 * \ingroup gc_major
 */
void gc_init_heap(long heap_size);

/**
 * \defgroup prim Primitives
 * @brief Built-in Scheme functions provided by the runtime library
 *
 */
/**@{*/

/**
 * \defgroup prim_err Error checking
 * @brief Runtime error checks including object type validation, bounds, and number of function arguments
 *
 */
/**@{*/
#define zyn_check_num_args(data, fnc_name, num_expected_args, args, args_len) { \
  if (num_expected_args > args_len) { \
    char buf[128]; \
    snprintf(buf, 127, "Expected %d arguments to %s but received %d", \
             num_expected_args, fnc_name, args_len);  \
    zyn_rt_raise_msg(data, buf); \
  } \
}

#define zyn_check_argc(data, fnc_name, argc, expected) { \
  if (expected > argc) { \
    char buf[128]; \
    snprintf(buf, 127, "Expected %d arguments to %s but received %d", \
             expected, fnc_name, argc);  \
    zyn_rt_raise_msg(data, buf); \
  } \
}

/**
 * Raise an error if obj is immutable
 * @param data - Thread data object
 * @param obj - Object to check
 */
#define zyn_verify_mutable(data, obj) { \
  if (immutable(obj)) zyn_immutable_obj_error(data, obj); }

/**
 * Raise an error if obj is mutable
 * @param data - Thread data object
 * @param obj - Object to check
 */
#define zyn_verify_immutable(data, obj) { \
  if (boolean_f == zyn_is_immutable(obj)) zyn_mutable_obj_error(data, obj); }

/**
 * Perform type checking and raise an error if the check fails.
 * @param data - Thread data object
 * @param fnc_test - Predicate to do type checking
 * @param tag - Object tag we are checking for
 * @param obj - Object to check
 */
#define zyn_check_type(data, fnc_test, tag, obj) { \
  if ((boolean_f == fnc_test(obj))) zyn_invalid_type_error(data, tag, obj); }
#define zyn_check_type2(data, fnc_test, tag, obj) { \
  if ((boolean_f == fnc_test(data, obj))) zyn_invalid_type_error(data, tag, obj); }

/**
 * Type Checking - raise an error unless `obj` is a pair object or NULL
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_pair_or_null(d,obj) { if (obj != NULL) { zyn_check_pair(d,obj); }}
/**
 * Type Checking - raise an error unless the object is a pair
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_pair(d,obj) zyn_check_type(d,zyn_is_pair, pair_tag, obj)
/**
 * Type Checking - raise an error unless the object is a procedure
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_proc(d,obj) zyn_check_type2(d,zyn_is_procedure, closureN_tag, obj)
/**
 * Type Checking - raise an error unless the object is a number
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_num(d,obj) zyn_check_type(d,zyn_is_number, integer_tag, obj)
/**
 * Type Checking - raise an error unless the object is an immediate integer
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_fixnum(d,obj) zyn_check_type(d,zyn_is_fixnum, integer_tag, obj)
/**
 * Type Checking - raise an error unless the object is an integer
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_int(d,obj) zyn_check_type(d,zyn_is_integer, integer_tag, obj)
/**
 * Type Checking - raise an error unless the object is a double precision number
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_double(d,obj) zyn_check_type(d,zyn_is_double, double_tag, obj)
/**
 * Type Checking - raise an error unless the object is a string
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_str(d,obj) zyn_check_type(d,zyn_is_string, string_tag, obj)
/**
 * Type Checking - raise an error unless the object is a symbol
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_sym(d,obj) zyn_check_type(d,zyn_is_symbol, symbol_tag, obj)
/**
 * Type Checking - raise an error unless the object is a vector
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_vec(d,obj) zyn_check_type(d,zyn_is_vector, vector_tag, obj)
/**
 * Type Checking - raise an error unless the object is a bytevector
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_bvec(d,obj) zyn_check_type(d,zyn_is_bytevector, bytevector_tag, obj)
/**
 * Type Checking - raise an error unless the object is a port
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_port(d,obj) zyn_check_type(d,zyn_is_port, port_tag, obj)
/**
 * Type Checking - raise an error unless the object is a mutex
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_mutex(d,obj) zyn_check_type(d,zyn_is_mutex, mutex_tag, obj)
/**
 * Type Checking - raise an error unless the object is a condition variable
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_cond_var(d,obj) zyn_check_type(d,zyn_is_cond_var, cond_var_tag, obj)
/**
 * Type Checking - raise an error unless the object is an atomic
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_atomic(d,obj) zyn_check_type(d,zyn_is_atomic, atomic_tag, obj)
/**
 * Type Checking - raise an error unless the object is an opaque
 * @param d - Thread data object
 * @param obj - Object to check
 */
#define zyn_check_opaque(d,obj) zyn_check_type(d,zyn_is_opaque, c_opaque_tag, obj)
void zyn_invalid_type_error(void *data, int tag, object found);
void zyn_immutable_obj_error(void *data, object obj);
void zyn_mutable_obj_error(void *data, object obj);
void zyn_check_obj(void *data, int tag, object obj);
void zyn_check_bounds(void *data, const char *label, int len, int index);
/**@}*/
/* END error checking */

extern long global_stack_size;
extern long global_heap_size;

char **get_env_variables();
void pack_env_variables(void *data, object k);
void set_env_variables(char **vars);

object cell_get(object cell);

#define global_set(glo,value) zyn_global_set(data, NULL, (object *)&glo, value)
#define global_set_id(id,glo,value) zyn_global_set(data, id, (object *)&glo, value)
object zyn_global_set(void *thd, object sym, object * glo, object value);

#define global_set_cps(thd,k,glo,value) zyn_global_set_cps(thd, k, NULL, (object *)&glo, value)
#define global_set_cps_id(thd,k,id,glo,value) zyn_global_set_cps(thd, k, id, (object *)&glo, value)
object zyn_global_set_cps(void *thd, object cont, object sym, object * glo,
                          object value);

/**
 * Variable argument count support
 *
 *   This macro is intended to be executed at the top of a function that
 *   is passed 'var' as a variable-length argument. 'count' is the number
 *   of varargs that were passed. EG:
 *   - C definition: f(object a, ...)
 *   - C call: f(1, 2, 3)
 *   - var: a
 *   - count: 3
 *
 *   Argument count would need to be passed by the caller of f. Presumably
 *   our compiler will compute the difference between the number of required
 *   args and the number of provided ones, and pass the difference as 'count'
 */
#define load_varargs(var, args_var, start, count) \
  list var = ((count) > 0) ? alloca(sizeof(pair_type)*(count)) : NULL; \
  { \
    int i; \
    object tmp; \
    if ((count) > 0) { \
      for (i = 0; i < (count); i++) { \
        tmp = args_var[start + i]; \
        var[i].hdr.mark = gc_color_red; \
        var[i].hdr.grayed = 0; \
        var[i].hdr.immutable = 0; \
        var[i].tag = pair_tag; \
        var[i].pair_car = tmp; \
        var[i].pair_cdr = (i == ((count)-1)) ? NULL : &var[i + 1]; \
      } \
    } \
  }
/* Prototypes for primitive functions. */

/**
 * \defgroup prim_ctrl Control flow
 * @brief Primitives that control the flow of program execution
 */

/**@{*/
object apply(void *data, object cont, object func, object args);
void zyn_apply(void *data, object cont, int argc, object * args);
void dispatch_apply_va(void *data, object clo, int argc, object * args);
object apply_va(void *data, object cont, int argc, object func, ...);
void dispatch(void *data, int argc, function_type func, object clo, object cont,
              object args);

/**@}*/

/**
 * \defgroup prim_str Strings
 * @brief String functions
 */
/**@{*/
object zyn_string_cmp(void *data, object str1, object str2);
void dispatch_string_91append(void *data, object clo, int _argc, object * args);
object zyn_string2number_(void *d, object cont, object str);
object zyn_string2number2_(void *data, object cont, int argc, object str, ...);
int binstr2int(const char *str);
int octstr2int(const char *str);
object zyn_string_append(void *data, object cont, int argc, object str1, ...);
object zyn_string_length(void *data, object str);
object zyn_string_byte_length(void *data, object str);
object zyn_substring(void *data, object cont, object str, object start,
                     object end);
object zyn_string_ref(void *data, object str, object k);
object zyn_string_set(void *data, object str, object k, object chr);
/**@}*/

/**
 * \defgroup prim_char Characters
 * @brief Character functions
 */
/**@{*/
object zyn_char2integer(object chr);
object zyn_char_eq_op(void *data, object a, object b);
object zyn_char_gt_op(void *data, object a, object b);
object zyn_char_lt_op(void *data, object a, object b);
object zyn_char_gte_op(void *data, object a, object b);
object zyn_char_lte_op(void *data, object a, object b);
/**@}*/

/**
 * \defgroup prim_sym Symbols
 * @brief Symbol functions
 */
/**@{*/
object zyn_symbol2string(void *d, object cont, object sym);
object zyn_string2symbol(void *d, object str);
/**@}*/

/**
 * \defgroup prim_cvar C vars
 * @brief Primitives for the C-variable integration type
 */
/**@{*/
extern object zyn_global_variables;
cvar_type *mcvar(object * var);
object zyn_get_global_variables();
object zyn_get_cvar(object var);
object zyn_set_cvar(object var, object value);
/**@}*/

/**
 * \defgroup prim_io I/O
 * @brief Input/Output functions
 */
/**@{*/
object zyn_display(void *data, object, FILE * port);
void dispatch_display_va(void *data, object clo, int argc, object * args);
object zyn_display_va(void *data, int argc, object x, ...);
object zyn_display_va_list(void *data, object x, object opts);
object zyn_write_char(void *data, object c, object port);
object zyn_write(void *data, object, FILE * port);
void dispatch_write_va(void *data, object clo, int argc, object * args);
object zyn_write_va(void *data, int argc, object x, ...);
object zyn_write_va_list(void *data, object x, object opts);
port_type zyn_stdout(void);
port_type zyn_stdin(void);
port_type zyn_stderr(void);
port_type zyn_io_open_input_file(void *data, object str);
port_type zyn_io_open_output_file(void *data, object str);
port_type zyn_io_open_binary_input_file(void *data, object str);
port_type zyn_io_open_binary_output_file(void *data, object str);
port_type *zyn_io_open_output_string(void *data);
port_type *zyn_io_open_input_string(void *data, object str);
port_type *zyn_io_open_input_bytevector(void *data, object bv);
void zyn_io_get_output_string(void *data, object cont, object port);
void zyn_io_get_output_bytevector(void *data, object cont, object port);
object zyn_io_close_port(void *data, object port);
object zyn_io_close_input_port(void *data, object port);
object zyn_io_close_output_port(void *data, object port);
object zyn_io_flush_output_port(void *data, object port);
object zyn_io_read_char(void *data, object cont, object port);
object zyn_io_peek_char(void *data, object cont, object port);
object zyn_io_char_ready(void *data, object port);
object zyn_write_u8(void *data, object c, object port);
object zyn_io_read_u8(void *data, object cont, object port);
object zyn_io_peek_u8(void *data, object cont, object port);
object zyn_write_bytevector(void *data, object bvec, object port, object start,
                            object end);
object zyn_io_read_line(void *data, object cont, object port);
void zyn_io_read_token(void *data, object cont, object port);
int zyn_have_mstreams();
/**@}*/

/**
 * \defgroup prim_num Numbers
 * @brief Number functions
 */
/**@{*/

/**
 * Extract result of OP and pass it in a call to continuation `cont`
 */
#define return_double_op(data, cont, OP, z) \
  int i = 0; \
  zyn_check_num(data, z); \
  if (obj_is_int(z)) { \
    i = obj_obj2int(z); \
  } else if (type_of(z) == integer_tag) { \
    i = (int)OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    return_closcall1(data, cont, z); \
  } else if (type_of(z) == double_tag) { \
    make_double(d, OP(((double_type *)z)->value)); \
    return_closcall1(data, cont, &d); \
  } else { \
    zyn_rt_raise2(data, "Expected number but received", z); \
  } \
  return_closcall1(data, cont, obj_int2obj(i));

/**
 * Directly return result of OP to caller
 */
#define return_double_op_no_cps(data, ptr, OP, z) \
  int i = 0; \
  zyn_check_num(data, z); \
  if (obj_is_int(z)) { \
    i = obj_obj2int(z); \
  } else if (type_of(z) == integer_tag) { \
    i = (int)OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    return z; \
  } else if (type_of(z) == double_tag) { \
    assign_double(ptr, OP(((double_type *)z)->value)); \
    return ptr; \
  } else { \
    zyn_rt_raise2(data, "Expected number but received", z); \
  } \
  return obj_int2obj(i);

/**
 * Extract double and return it to caller
 */
#define return_inexact_double_op_no_cps(data, ptr, OP, z) \
  double unboxed; \
  zyn_check_num(data, z); \
  if (obj_is_int(z)) { \
    unboxed = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    unboxed = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    unboxed = OP(mp_get_double(&bignum_value(z))); \
  } else { \
    unboxed = OP(((double_type *)z)->value); \
  } \
  assign_double(ptr, unboxed); \
  return ptr;

/**
 * Extract double and pass it to continuation cont
 */
#define return_inexact_double_op(data, cont, OP, z) \
  make_double(d, 0.0); \
  zyn_check_num(data, z); \
  if (obj_is_int(z)) { \
    d.value = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    d.value = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    d.value = OP(mp_get_double(&bignum_value(z))); \
  } else { \
    d.value = OP(((double_type *)z)->value); \
  } \
  return_closcall1(data, cont, &d)

/**
 * Extract double or complex number and return it to caller
 */
#define return_inexact_double_or_cplx_op_no_cps(data, ptr, OP, CPLX_OP, z) \
  double unboxed; \
  zyn_check_num(data, z); \
  if (obj_is_int(z)) { \
    unboxed = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    unboxed = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    unboxed = OP(mp_get_double(&bignum_value(z))); \
  } else if (type_of(z) == complex_num_tag) { \
    double complex unboxed = CPLX_OP(complex_num_value(z)); \
    assign_complex_num(ptr, unboxed); \
    return ptr; \
  } else { \
    unboxed = OP(((double_type *)z)->value); \
  } \
  assign_double(ptr, unboxed); \
  return ptr;

/**
 * Extract double or complex number and pass it in a call to continuation `cont`
 */
#define return_inexact_double_or_cplx_op(data, cont, OP, CPLX_OP, z) \
  make_double(d, 0.0); \
  zyn_check_num(data, z); \
  if (obj_is_int(z)) { \
    d.value = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    d.value = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    d.value = OP(mp_get_double(&bignum_value(z))); \
  } else if (type_of(z) == complex_num_tag) { \
    complex_num_type cn; \
    double complex unboxed = CPLX_OP(complex_num_value(z)); \
    assign_complex_num((&cn), unboxed); \
    return_closcall1(data, cont, &cn); \
  } else { \
    d.value = OP(((double_type *)z)->value); \
  } \
  return_closcall1(data, cont, &d)

double round_to_nearest_even(double);
void zyn_exact(void *data, object cont, object z);
object zyn_exact_no_cps(void *data, object ptr, object z);

/**
 * Take Scheme object that is a number and return the number as a C type
 */
#define unbox_number(n) \
  ((obj_is_int(n) ? obj_obj2int(n) : \
                    ((type_of(n) == integer_tag) ? \
                       ((integer_type *)n)->value : \
                       ((double_type *)n)->value)))

object zyn_num_eq(void *, object cont, int argc, object n, ...);
object zyn_num_gt(void *, object cont, int argc, object n, ...);
object zyn_num_lt(void *, object cont, int argc, object n, ...);
object zyn_num_gte(void *, object cont, int argc, object n, ...);
object zyn_num_lte(void *, object cont, int argc, object n, ...);
int zyn_num_eq_op(void *, object x, object y);
int zyn_num_gt_op(void *, object x, object y);
int zyn_num_lt_op(void *, object x, object y);
int zyn_num_gte_op(void *, object x, object y);
int zyn_num_lte_op(void *, object x, object y);
object zyn_num_fast_eq_op(void *data, object x, object y);
object zyn_num_fast_gt_op(void *data, object x, object y);
object zyn_num_fast_lt_op(void *data, object x, object y);
object zyn_num_fast_gte_op(void *data, object x, object y);
object zyn_num_fast_lte_op(void *data, object x, object y);
object zyn_num_cmp_va_list(void *data, int argc,
                           int (fn_op(void *, object, object)), object n,
                           va_list ns);
void zyn_expt(void *data, object cont, object x, object y);
void zyn_remainder(void *data, object cont, object num1, object num2);
void zyn_get_ratio(void *data, object cont, object n, int numerator);
object zyn_number2string2(void *data, object cont, int argc, object n, ...);
object zyn_integer2char(void *data, object n);
object zyn_sum_op(void *data, common_type * x, object y);
object zyn_sub_op(void *data, common_type * x, object y);
object zyn_mul_op(void *data, common_type * x, object y);
object zyn_div_op(void *data, common_type * x, object y);
object zyn_sum(void *data, object cont, int argc, object n, ...);
object zyn_sub(void *data, object cont, int argc, object n, ...);
object zyn_mul(void *data, object cont, int argc, object n, ...);
object zyn_div(void *data, object cont, int argc, object n, ...);
// Future idea, there may be uses for this in addition to if statements:
#define zyn_if(c,t,e) ((boolean_f != c) ? (t) : (e))
object zyn_fast_sum(void *data, object ptr, object x, object y);
object zyn_fast_sub(void *data, object ptr, object x, object y);
object zyn_fast_mul(void *data, object ptr, object x, object y);
object zyn_fast_div(void *data, object ptr, object x, object y);
object zyn_fast_list_2(object ptr, object x, object y);
object zyn_fast_list_3(object ptr, object a1, object a2, object a3);
object zyn_fast_list_4(object ptr, object a1, object a2, object a3, object a4);
object zyn_fast_vector_2(object ptr, object a1, object a2);
object zyn_fast_vector_3(object ptr, object a1, object a2, object a3);
object zyn_fast_vector_4(object ptr, object a1, object a2, object a3,
                         object a4);
object zyn_fast_vector_5(object ptr, object a1, object a2, object a3, object a4,
                         object a5);
object zyn_bit_unset(void *data, object n1, object n2);
object zyn_bit_set(void *data, object n1, object n2);
object zyn_num_op_va_list(void *data, int argc,
                          object(fn_op(void *, common_type *, object)),
                          int default_no_args, int default_one_arg, object n,
                          va_list ns, common_type * buf);
object zyn_num_op_args(void *data, int argc,
                       object(fn_op(void *, common_type *, object)),
                       int default_no_args, int default_one_arg,
                       object * args, common_type * buf);
void zyn_int2bignum(int n, mp_int * bn);
object zyn_bignum_normalize(void *data, object n);
int zyn_bignum_cmp(bn_cmp_type type, object x, int tx, object y, int ty);
void zyn_make_rectangular(void *data, object k, object r, object i);
double MRG32k3a(double seed);
/**@}*/
/**
 * \defgroup prim_eq Equality and type predicates
 */
/**@{*/
//object zyn_eq(object x, object y);
object zyn_eqv(object x, object y);
#define zyn_eq(x, y) (make_boolean(x == y))
object equalp(object, object);
object zyn_has_cycle(object lst);
object zyn_is_list(object lst);
//object zyn_is_boolean(object o);
#define zyn_is_boolean(o) (make_boolean(o == boolean_f || o == boolean_t))
#define zyn_is_pair(o) ((is_object_type(o) && ((list) o)->tag == pair_tag) ? boolean_t : boolean_f)
#define zyn_is_null(o) (make_boolean(o == NULL))
//TODO: convert all of these to macros (if it makes sense, most should), and remove them from runtime.c:
object zyn_is_number(object o);
object zyn_is_real(object o);
object zyn_is_integer(object o);
#define zyn_is_fixnum(o) (make_boolean(obj_is_int(o)))
//object zyn_is_fixnum(object o);
#define zyn_is_double(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == double_tag))
#define zyn_is_bignum(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == bignum_tag))
//object zyn_is_complex(object o);
#define zyn_is_complex(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == complex_num_tag))
//object zyn_is_bignum(object o);
//object zyn_is_vector(object o);
//object zyn_is_bytevector(object o);
//object zyn_is_port(object o);
//object zyn_is_mutex(object o);
//object zyn_is_cond_var(object o);
//object zyn_is_symbol(object o);
//object zyn_is_string(object o);
object zyn_is_record(object o);
#define zyn_is_vector_not_record_type(o)     \
  (make_boolean(is_object_type(o) && \
                ((vector) o)->tag == vector_tag && \
                ( ((vector) o)->num_elements == 0 || \
                  ((vector) o)->elements[0] != zyn_RECORD_MARKER ) \
                ))
#define zyn_is_vector(o)     (make_boolean(is_object_type(o) && ((vector) o)->tag == vector_tag))
#define zyn_is_bytevector(o) (make_boolean(is_object_type(o) && ((list) o)->tag == bytevector_tag))
#define zyn_is_port(o)       (make_boolean(is_object_type(o) && ((list) o)->tag == port_tag))
#define zyn_is_mutex(o)      (make_boolean(is_object_type(o) && ((list) o)->tag == mutex_tag))
#define zyn_is_atomic(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == atomic_tag))
#define zyn_is_cond_var(o)   (make_boolean(is_object_type(o) && ((list) o)->tag == cond_var_tag))
#define zyn_is_symbol(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == symbol_tag))
#define zyn_is_string(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == string_tag))
//object zyn_is_char(object o);
#define zyn_is_char(o) (make_boolean(obj_is_char(o)))
object zyn_is_procedure(void *data, object o);
//object zyn_is_macro(object o);
//object zyn_is_eof_object(object o);
//object zyn_is_cvar(object o);
//object zyn_is_opaque(object o);
#define zyn_is_macro(o)       (make_boolean(is_object_type(o) && ((list) o)->tag == macro_tag))
#define zyn_is_eof_object(o)  (make_boolean(is_object_type(o) && ((list) o)->tag == eof_tag))
#define zyn_is_void_object(o) (make_boolean(is_object_type(o) && ((list) o)->tag == void_tag))
#define zyn_is_cvar(o)        (make_boolean(is_object_type(o) && ((list) o)->tag == cvar_tag))
#define zyn_is_opaque(o)      (make_boolean(is_object_type(o) && ((list) o)->tag == c_opaque_tag))
object zyn_is_immutable(object obj);
/**@}*/

/**
 * \defgroup prim_vec Vectors
 * @brief Vector functions
 */
/**@{*/
object zyn_vector_length(void *data, object v);
object zyn_vector_ref(void *d, object v, object k);
#define zyn_vector_ref_unsafe(d, v, k) \
  ((vector) v)->elements[obj_obj2int(k)]
object zyn_vector_set(void *d, object v, object k, object obj);
object zyn_vector_set_unsafe(void *d, object v, object k, object obj);
object zyn_vector_set_cps(void *d, object cont, object v, object k, object obj);
object zyn_vector_set_unsafe_cps(void *d, object cont, object v, object k,
                                 object obj);
object zyn_make_vector(void *data, object cont, int argc, object len, ...);
/**@}*/

/**
 * \defgroup prim_bv Bytevectors
 * @brief Bytevector functions
 */
/**@{*/
object zyn_make_bytevector(void *data, object cont, int argc, object len, ...);
object zyn_bytevector(void *data, object cont, int argc, object bval, ...);
object zyn_bytevector_length(void *data, object bv);
object zyn_bytevector_append(void *data, object cont, int _argc, object bv,
                             ...);
object zyn_bytevector_copy(void *data, object cont, object bv, object start,
                           object end);
object zyn_bytevector_u8_ref(void *data, object bv, object k);
object zyn_bytevector_u8_set(void *data, object bv, object k, object b);
object zyn_utf82string(void *data, object cont, object bv, object start,
                       object end);
object zyn_string2utf8(void *data, object cont, object str, object start,
                       object end);
/**@}*/

/**
 * \defgroup prim_sys System interface
 * @brief Functions for interacting with the system
 */
/**@{*/
extern int _cyc_argc;
extern char **_cyc_argv;
object zyn_installation_dir(void *data, object cont, object type);
/* object zyn_compilation_environment(void *data, object cont, object var); */
object zyn_command_line_arguments(void *data, object cont);
object zyn_system(object cmd);
void zyn_exit(void *data, object clo, int argc, object * args);
object __halt(object obj);
object zyn_io_delete_file(void *data, object filename);
object zyn_io_file_exists(void *data, object filename);
time_t zyn_file_last_modified_time(char *path);
/**@}*/

/**
 * \defgroup prim_thd Threads
 * @brief Thread-oriented functions
 *
 * Most of these are internal and should not be called from
 * an FFI function.
 */
/**@{*/
object zyn_spawn_thread(object thunk);
void zyn_start_trampoline(gc_thread_data * thd);
void zyn_end_thread(gc_thread_data * thd);
void zyn_exit_thread(void *data, object _, int argc, object * args);
object zyn_thread_sleep(void *data, object timeout);
/**@}*/

/**
 * \defgroup prim_gc Garbage collection
 * @brief Functions to manually trigger a GC
 */
/**@{*/
object zyn_trigger_minor_gc(void *data, object cont);
object copy2heap(void *data, object obj);
/**@}*/

/**
 * \defgroup prim_ch Call history
 *
 * @brief Functions for maintaining call history.
 */
/**@{*/

//void zyn_st_add(void *data, char *frame); migrated from runtime.c
/**
 * @brief Register a frame in the stack trace circular buffer.
 * @param data Thread data object
 * @param frame Name of the frame
 */
#define zyn_st_add(data, frame) \
{ \
  gc_thread_data *thd = (gc_thread_data *) data; \
  intptr_t p1 = (intptr_t)frame; \
  intptr_t p2 = (intptr_t)thd->stack_prev_frame; \
  /* Do not allow recursion to remove older frames */ \
  if (p1 != p2) { \
    thd->stack_prev_frame = frame; \
    thd->stack_traces[thd->stack_trace_idx] = frame; \
    thd->stack_trace_idx = (thd->stack_trace_idx + 1) % MAX_STACK_TRACES; \
  } \
}

void zyn_st_print(void *data, FILE * out);
/**@}*/

/**
 * \defgroup prim_obj Primitive objects
 *
 * @brief Objects added to the global environment at runtime as references to the corresponding primitives.
 *
 * This code was originally auto-generated via `--autogen`
 */
/**@{*/
extern const object primitive_zyn_91global_91vars;
extern const object primitive_zyn_91get_91cvar;
extern const object primitive_zyn_91set_91cvar_67;
extern const object primitive_zyn_91cvar_127;
extern const object primitive_zyn_91opaque_127;
extern const object primitive_zyn_91has_91cycle_127;
extern const object primitive_zyn_91spawn_91thread_67;
extern const object primitive_zyn_91end_91thread_67;
extern const object primitive__87;
extern const object primitive__91;
extern const object primitive__85;
extern const object primitive__95;
extern const object primitive__123;
extern const object primitive__125;
extern const object primitive__121;
extern const object primitive__125_123;
extern const object primitive__121_123;
extern const object primitive_apply;
extern const object primitive__75halt;
extern const object primitive_exit;
extern const object primitive_zyn_91current_91exception_91handler;
extern const object primitive_zyn_91default_91exception_91handler;
extern const object primitive_cons;
extern const object primitive_cell_91get;
extern const object primitive_set_91global_67;
extern const object primitive_set_91cell_67;
extern const object primitive_cell;
extern const object primitive_eq_127;
extern const object primitive_eqv_127;
extern const object primitive_equal_127;
extern const object primitive_assq;
extern const object primitive_assv;
extern const object primitive_memq;
extern const object primitive_memv;
extern const object primitive_length;
extern const object primitive_vector_91length;
extern const object primitive_bytevector_91length;
extern const object primitive_set_91car_67;
extern const object primitive_set_91cdr_67;
extern const object primitive_car;
extern const object primitive_cdr;
extern const object primitive_caar;
extern const object primitive_cadr;
extern const object primitive_cdar;
extern const object primitive_cddr;
extern const object primitive_caaar;
extern const object primitive_caadr;
extern const object primitive_cadar;
extern const object primitive_caddr;
extern const object primitive_cdaar;
extern const object primitive_cdadr;
extern const object primitive_cddar;
extern const object primitive_cdddr;
extern const object primitive_caaaar;
extern const object primitive_caaadr;
extern const object primitive_caadar;
extern const object primitive_caaddr;
extern const object primitive_cadaar;
extern const object primitive_cadadr;
extern const object primitive_caddar;
extern const object primitive_cadddr;
extern const object primitive_cdaaar;
extern const object primitive_cdaadr;
extern const object primitive_cdadar;
extern const object primitive_cdaddr;
extern const object primitive_cddaar;
extern const object primitive_cddadr;
extern const object primitive_cdddar;
extern const object primitive_cddddr;
extern const object primitive_char_91_125integer;
extern const object primitive_integer_91_125char;
extern const object primitive_string_91_125number;
extern const object primitive_string_91cmp;
extern const object primitive_string_91append;
extern const object primitive_list_91_125string;
extern const object primitive_string_91_125symbol;
extern const object primitive_symbol_91_125string;
extern const object primitive_number_91_125string;
extern const object primitive_string_91length;
extern const object primitive_substring;
extern const object primitive_make_91bytevector;
extern const object primitive_make_91vector;
extern const object primitive_list_91_125vector;
extern const object primitive_vector_91ref;
extern const object primitive_vector_91set_67;
extern const object primitive_bytevector;
extern const object primitive_bytevector_91append;
extern const object primitive_zyn_91bytevector_91copy;
extern const object primitive_zyn_91string_91_125utf8;
extern const object primitive_zyn_91utf8_91_125string;
extern const object primitive_bytevector_91u8_91ref;
extern const object primitive_bytevector_91u8_91set_67;
extern const object primitive_string_91ref;
extern const object primitive_string_91set_67;
extern const object primitive_zyn_91installation_91dir;
extern const object primitive_zyn_91compilation_91environment;
extern const object primitive_command_91line_91arguments;
extern const object primitive_system;
extern const object primitive_boolean_127;
extern const object primitive_char_127;
extern const object primitive_eof_91object_127;
extern const object primitive_null_127;
extern const object primitive_number_127;
extern const object primitive_real_127;
extern const object primitive_integer_127;
extern const object primitive_pair_127;
extern const object primitive_procedure_127;
extern const object primitive_macro_127;
extern const object primitive_zyn_91macro_127;
extern const object primitive_port_127;
extern const object primitive_vector_127;
extern const object primitive_bytevector_127;
extern const object primitive_string_127;
extern const object primitive_symbol_127;
extern const object primitive_open_91input_91file;
extern const object primitive_open_91output_91file;
extern const object primitive_open_91binary_91input_91file;
extern const object primitive_open_91binary_91output_91file;
extern const object primitive_close_91port;
extern const object primitive_close_91input_91port;
extern const object primitive_close_91output_91port;
extern const object primitive_zyn_91flush_91output_91port;
extern const object primitive_file_91exists_127;
extern const object primitive_delete_91file;
extern const object primitive_read_91char;
extern const object primitive_peek_91char;
extern const object primitive_zyn_91read_91line;
extern const object primitive_zyn_91write_91char;
extern const object primitive_zyn_91write;
extern const object primitive_zyn_91display;
extern const object primitive_call_95cc;
/* -------------------------------------------- */
/**@}*/

/** Globals that are needed by the runtime
 *  What's going on here is the globals are defined by a module, but
 *  are also used by the runtime. At least for now, macros below are
 *  used to point everybody to the objects.
 *
 *  The assumption for now is that a program that does not include
 *  the necessary libray would never use the corresponding function.
 */
extern object zyn_glo_eval_from_c;
extern object zyn_glo_call_cc;

#define __glo_eval_91from_91c_scheme_eval zyn_glo_eval_from_c
#define __glo_call_95cc_scheme_base zyn_glo_call_cc

/**
 * \defgroup prim_ex Exception handling
 * @brief Raise and handle Scheme exceptions
 */
/**@{*/
object zyn_default_exception_handler(void *data, object _, int argc,
                                     object * args);

object zyn_current_exception_handler(void *data);
void zyn_rt_raise(void *data, object err);
void zyn_rt_raise2(void *data, const char *msg, object err);
void zyn_rt_raise_msg(void *data, const char *err);
/**@}*/

/**@}*/

/**
 * \defgroup prim_symtbl Symbol table
 *
 * @brief The symbol table, a thread-safe container for all symbols.
 *
 * This table contains a pointer to each symbol used by the current
 * program.
 */
/**@{*/
object add_symbol(symbol_type * psym);
object find_or_add_symbol(const char *name);
/**@}*/

/**
 * \defgroup prim_glo Library table
 *
 * @brief A table of scheme libraries that are loaded.
 */
/**@{*/
object is_library_loaded(const char *name);
object register_library(const char *name);
/**@}*/

/**
 * \defgroup prim_glo Global table
 *
 * @brief A table of global variables.
 */
/**@{*/
extern list global_table;
void add_global(const char *identifier, object * glo);
void zyn_set_globals_changed(gc_thread_data * thd);
/**@}*/

/**
 * \defgroup prim_utf8 UTF-8
 *
 * @brief Unicode processing using UTF-8
 */
/**@{*/

/** @brief Successful state */
#define ZYN_UTF8_ACCEPT 0

/** @brief Invalid state */
#define ZYN_UTF8_REJECT 1

/**
 * Simple macro to make it more convenient to convert a single char
 */
#define zyn_utf8_encode_char(dest, dest_size, char_value) \
  zyn_utf8_encode(dest, dest_size, &char_value, 1)

int zyn_utf8_encode(char *dest, int sz, uint32_t * src, int srcsz);
int zyn_utf8_count_code_points(uint8_t * s);
uint32_t zyn_utf8_validate_stream(uint32_t * state, char *str, size_t len);
uint32_t zyn_utf8_validate(char *str, size_t len);
/**@}*/

/**
 * \defgroup prim_pairs Pairs and lists
 * @brief Functions for working with pairs and lists
 */
/**@{*/
//object zyn_car(void *data, object lis);
//object zyn_cdr(void *data, object lis);
static inline object zyn_car(void *data, object lis)
{
  zyn_check_pair(data, lis);
  return car(lis);
}

static inline object zyn_cdr(void *data, object lis)
{
  zyn_check_pair(data, lis);
  return cdr(lis);
}

// Unsafe car/cdr
#define zyn_car_unsafe(d, lis) car(lis)
#define zyn_cdr_unsafe(d, lis) cdr(lis)

list malloc_make_pair(object, object);
object zyn_set_cell(void *, object l, object val);
object zyn_set_car(void *, object l, object val);
object zyn_set_cdr(void *, object l, object val);
object zyn_set_car_cps(void *, object cont, object l, object val);
object zyn_set_cdr_cps(void *, object cont, object l, object val);
object zyn_length(void *d, object l);
object zyn_length_unsafe(void *d, object l);
object zyn_list2vector(void *data, object cont, object l);
object zyn_list2string(void *d, object cont, object lst);
object memberp(void *data, object x, list l);
object memvp(void *data, object x, list l);
object memqp(void *data, object x, list l);
list assq(void *data, object x, list l);
list assv(void *data, object x, list l);
list assoc(void *data, object x, list l);
list assoc_cdr(void *data, object x, list l);
/**@}*/

void init_polyfills(void);

#endif // ZAYIN_H
