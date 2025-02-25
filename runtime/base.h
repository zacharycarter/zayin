#ifndef ZAYIN_H
#define ZAYIN_H

#include <pthread.h>
#include <setjmp.h>
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

enum __attribute__((__packed__)) object_tag {
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
  enum object_tag tag;
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

struct obj object_base_new(enum object_tag);
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


// NEW Implementation

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
enum zyn_object_tag {
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
/* #define mark(x) (((list) x)->hdr.mark) */

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
} zyn_thread_state_type;

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
  zyn_thread_state_type thread_state;
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

/** Determine if stack has overflowed */
#if STACK_GROWTH_IS_DOWNWARD
#define stack_overflow(x,y) ((x) < (y))
#else
#define stack_overflow(x,y) ((x) > (y))
#endif


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
 * The boolean True value.
 * \ingroup objects
 */
extern const object boolean_t;

/**
 * \ingroup gc_minor
 */
void GC(void *, closure, object *, int);

/**
 * \ingroup gc_major
 */
void gc_init_heap(long heap_size);


void gc_initialize(void);
void gc_add_mutator(gc_thread_data * thd);
gc_heap *gc_heap_create(int heap_type, size_t size, gc_thread_data * thd);

void gc_thr_grow_move_buffer(gc_thread_data * d);
void gc_thread_data_init(gc_thread_data * thd, int mut_num, char *stack_base,
                         long stack_size);

void gc_start_collector();

extern long global_stack_size;
extern long global_heap_size;

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

#endif /* ZAYIN_H */
