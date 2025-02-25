#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <mimalloc.h>
#include <ck_array.h>

#include "base.h"
#include "common.h"
#include "gc.h"
#include "hash_table.h"
#include "queue.h"
#include "vec.h"

MAKE_VECTOR(struct obj *, gc_heap_nodes);
MAKE_VECTOR(size_t, size_t);
MAKE_QUEUE(struct obj *, gc_grey_nodes);
MAKE_QUEUE(struct ptr_toupdate_pair, ptr_toupdate_pair);

bool size_t_eq(size_t a, size_t b) { return a == b; }

MAKE_HASH(size_t, struct obj *, hash_table_default_size_t_hash_fun, size_t_eq,
          ptr_map);

static struct gc_data gc_global_data;

// array of gc_funcs for each object type
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



// This does nothing, the gc will call free() on the object if it was heap
// allocated
void gc_free_noop(struct obj *obj) { (void)obj; }

// This does nothing, for objects where marking them should mark them as black
// and nothing more
void gc_mark_noop(struct obj *obj, struct gc_context *ctx) {
  (void)obj;
  (void)ctx;
}

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

struct gc_context gc_make_context(void) {
  return (struct gc_context){
      .grey_nodes = queue_gc_grey_nodes_new(10),
      .pointers_toupdate = queue_ptr_toupdate_pair_new(10),
      .updated_pointers = hash_table_ptr_map_new(),
  };
}

void gc_free_context(struct gc_context *ctx) {
  queue_gc_grey_nodes_free(&ctx->grey_nodes);
  queue_ptr_toupdate_pair_free(&ctx->pointers_toupdate);
  hash_table_ptr_map_free(ctx->updated_pointers);
}

void gc_mark_obj(struct gc_context *ctx, struct obj *obj) {
  obj->mark = BLACK;
  gc_func_map[obj->tag].mark(obj, ctx);
}

// Moves all live objects on the stack over to the heap
struct obj *gc_toheap(struct gc_context *ctx, struct obj *obj) {
  if (!obj) {
    return NULL;
  }

  // if we've already copied this object,
  // we know that anything it points to must also be sorted
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

  // mark the object as now being on the heap
  new_obj->on_stack = false;

  // Add it to the updated map
  // Even if it was on the heap already we still insert
  // since we then won't process child objects further
  hash_table_ptr_map_insert(ctx->updated_pointers, (size_t)obj, new_obj);

  return new_obj;
}

// The minor gc, moves all stack objects to the heap
// The parameter 'thnk' is the current thunk holding everything together
// The thunk should be heap allocated and freed after being called
void gc_minor(struct gc_context *ctx, struct thunk *thnk) {
  DEBUG_FPRINTF(stderr, "minor gc occuring\n");

  // initially mark the closure and it's arguments to be applied
  thnk->closr = (struct closure_obj *)gc_toheap(ctx, (struct obj *)thnk->closr);

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

  // work through each pointer that needs to be updated
  while (queue_ptr_toupdate_pair_len(&ctx->pointers_toupdate) > 0) {
    struct ptr_toupdate_pair to_update =
        queue_ptr_toupdate_pair_dequeue(&ctx->pointers_toupdate);

    struct obj **maybe_copied = hash_table_ptr_map_lookup(
        ctx->updated_pointers, (size_t)to_update.on_stack);

    if (maybe_copied != NULL) {
      // we've already updated this pointer, just update the pointer that
      // needs to be updated
      *to_update.toupdate = *maybe_copied;
    } else {
      // we haven't seen this yet, perform a copy and update

      assert(to_update.on_stack != NULL);

      struct obj *on_heap = gc_toheap(ctx, to_update.on_stack);
      hash_table_ptr_map_insert(ctx->updated_pointers,
                                (size_t)to_update.on_stack, on_heap);
      *to_update.toupdate = on_heap;
    }
  }

  gc_major(ctx, thnk);
}

// The major gc, collects objects on the heap
void gc_major(struct gc_context *ctx, struct thunk *thnk) {
  size_t num_freed = 0;
  size_t num_marked = 0;

  gc_mark_obj(ctx, &thnk->closr->base);
  num_marked++;

  switch (thnk->closr->size) {
  case CLOSURE_ONE:
    if (thnk->one.rand)
      gc_mark_obj(ctx, thnk->one.rand);
    num_marked++;
    break;
  case CLOSURE_TWO:
    if (thnk->two.rand)
      gc_mark_obj(ctx, thnk->two.rand);
    if (thnk->two.cont)
      gc_mark_obj(ctx, thnk->two.cont);
    num_marked++;
    num_marked++;
    break;
  }

  while (queue_gc_grey_nodes_len(&ctx->grey_nodes) > 0) {
    struct obj *next_obj = queue_gc_grey_nodes_dequeue(&ctx->grey_nodes);
    if (DEBUG_ONLY(!next_obj)) {
      RUNTIME_ERROR("NULL was added to mark queue!");
    }
    gc_mark_obj(ctx, next_obj);
    num_marked++;
  }

  DEBUG_FPRINTF("marked %zu objects\n", num_marked);

#ifdef DEBUG
  int seen_types[LAST_OBJ_TYPE] = {0};
#endif

  // go through each heap allocated object and gc them
  // not really the best, but it would be easy to improve
  for (size_t i = 0; i < gc_global_data.nodes.length; i++) {
    struct obj **ptr = vector_gc_heap_nodes_index_ptr(&gc_global_data.nodes, i);
    struct obj *obj = *ptr;

    if (obj == NULL) {
      continue;
    }

#ifdef DEBUG
    seen_types[obj->tag - 1]++;
#endif

    if (obj->mark == WHITE) {
      // free it, should this be done if the object is on the stack?
      if (DEBUG_ONLY(obj->on_stack)) {
        DEBUG_ONLY(RUNTIME_ERROR(
            "Object (%p, tag: %d, %s) was on the stack during a major GC!",
            (void *)obj, obj->tag, obj->last_touched_by));
      }

      // execute this object's free function
      gc_func_map[obj->tag].free(obj);

      mi_free(obj);
      num_freed++;

      // set the pointer in the vector to null
      *ptr = NULL;
    } else if (DEBUG_ONLY(obj->mark == GREY)) {
      // this shouldn't happen, but just incase
      RUNTIME_ERROR("Object was marked grey at time of major GC!");
    } else {
      // reset marker now
      obj->mark = WHITE;
    }
  }

  DEBUG_FPRINTF(stderr, "freed %zu objects\n", num_freed);
  DEBUG_FPRINTF(stderr, "size of heap nodes: %zu\n",
                gc_global_data.nodes.length);

#ifdef DEBUG
  for (int i = 0; i < LAST_OBJ_TYPE; i++) {
    printf("tag %d seen %d times\n", i + 1, seen_types[i]);
  }
#endif

  gc_heap_maintain();
}

void gc_init(void)
{
  gc_global_data.nodes = vector_gc_heap_nodes_new(100);
}

// wrapped malloc that adds allocated stuff to the bookkeeper
void *gc_malloc(size_t size) {
  void *ptr = mi_malloc(size);

  vector_gc_heap_nodes_push(&gc_global_data.nodes, ptr);
  return ptr;
}

void gc_heap_maintain(void) {
  size_t last_i = 0;
  size_t original_len = gc_global_data.nodes.length;
  for (size_t i = 0; i < gc_global_data.nodes.length; i++) {
    struct obj *obj = vector_gc_heap_nodes_index(&gc_global_data.nodes, i);
    if (obj != NULL) {
      vector_gc_heap_nodes_set(&gc_global_data.nodes, obj, last_i++);
    }
  }

  gc_global_data.nodes.length = last_i;
  if (last_i && (original_len / last_i) > 2)
    vector_gc_heap_nodes_shrink_to_fit(&gc_global_data.nodes);
}

// NEW Implementation

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

// Generic buffer functions
void **vpbuffer_realloc(void **buf, int *len)
{
  return realloc(buf, (*len) * sizeof(void *));
}

void **vpbuffer_add(void **buf, int *len, int i, void *obj)
{
  if (i == *len) {
    *len *= 2;
    buf = vpbuffer_realloc(buf, len);
  }
  buf[i] = obj;
  return buf;
}

void vpbuffer_free(void **buf)
{
  free(buf);
}

vpbuffer *vp_create(void)
{
  vpbuffer *v = malloc(sizeof(vpbuffer));
  v->len = 128;
  v->count = 0;
  v->buf = NULL;
  v->buf = vpbuffer_realloc(v->buf, &(v->len));
  return v;
}

void vp_add(vpbuffer * v, void *obj)
{
  v->buf = vpbuffer_add(v->buf, &(v->len), v->count++, obj);
}

////////////////////
// Global defines

// 64-bit is 3, 32-bit is 2
#define GC_BLOCK_BITS 5

/* HEAP definitions, based off heap from Chibi scheme */
#define gc_heap_first_block(h) ((object)(h->data + gc_heap_align(gc_free_chunk_size)))
#define gc_heap_end(h) ((object)((char*)h->data + h->size))
#define gc_heap_pad_size(s) (sizeof(struct gc_heap_t) + (s) + gc_heap_align(1))
#define gc_free_chunk_size (sizeof(gc_free_list))

#define gc_align(n, bits) (((n)+(1<<(bits))-1)&(((uintptr_t)-1)-((1<<(bits))-1)))

// Align to 8 byte block size (EG: 8, 16, etc)
#define gc_word_align(n) gc_align((n), 3)

// Align on GC_BLOCK_BITS, currently block size of 32 bytes
#define gc_heap_align(n) gc_align(n, GC_BLOCK_BITS)

////////////////////
// Global variables

// Note: will need to use atomics and/or locking to access any
// variables shared between threads
static unsigned char gc_color_mark = 5; // Black, is swapped during GC
static unsigned char gc_color_clear = 3;        // White, is swapped during GC
static unsigned char gc_color_purple = 1;       // There are many "shades" of purple, this is the most recent one
// unfortunately this had to be split up; const colors are located in types.h

static int gc_status_col = STATUS_SYNC1;
static int gc_stage = STAGE_RESTING;
static int gc_threads_merged = 0;

static void **mark_stack = NULL;
static int mark_stack_len = 0;
static int mark_stack_i = 0;

// Data for the "main" thread which is guaranteed to always be there.
static gc_thread_data *primordial_thread = NULL;

static ck_array_t new_mutators;
static ck_array_t zyn_mutators;
static ck_array_t old_mutators;
static pthread_mutex_t mutators_lock;

static void zyn_free(void *p, size_t m, bool d)
{
  mi_free(p);
  return;
}

static void *zyn_malloc(size_t b)
{
  return mi_malloc(b);
}

static void *zyn_realloc(void *r, size_t a, size_t b, bool d)
{
  return mi_realloc(r, b);
}

static struct ck_malloc zyn_allocator = {
  .malloc = zyn_malloc,
  .free = zyn_free,
  .realloc = zyn_realloc
};

/** Mark buffers
 *
 * For these, we need a buffer than can grow as needed but that can also be
 * used concurrently by both a mutator thread and a collector thread.
 */

static mark_buffer *mark_buffer_init(unsigned initial_size)
{
  mark_buffer *mb = malloc(sizeof(mark_buffer));
  mb->buf = malloc(sizeof(void *) * initial_size);
  mb->buf_len = initial_size;
  mb->next = NULL;
  return mb;
}

/////////////
// Functions

/**
 * @brief Perform one-time initialization before mutators can be executed
 */
void gc_initialize(void)
{
  if (ck_array_init(&zyn_mutators, CK_ARRAY_MODE_SPMC, &zyn_allocator, 10) == 0) {
    fprintf(stderr, "Unable to initialize mutator array\n");
    exit(1);
  }
  if (ck_array_init(&new_mutators, CK_ARRAY_MODE_SPMC, &zyn_allocator, 10) == 0) {
    fprintf(stderr, "Unable to initialize mutator array\n");
    exit(1);
  }
  if (ck_array_init(&old_mutators, CK_ARRAY_MODE_SPMC, &zyn_allocator, 10) == 0) {
    fprintf(stderr, "Unable to initialize mutator array\n");
    exit(1);
  }

  // Initialize collector's mark stack
  mark_stack_len = 128;
  mark_stack = vpbuffer_realloc(mark_stack, &(mark_stack_len));

  // Here is as good a place as any to do this...
  if (pthread_mutex_init(&(mutators_lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize mutators_lock mutex\n");
    exit(1);
  }
}

/**
 * @brief  Add data for a new mutator that is starting to run.
 * @param  thd  Thread data for the mutator
 */
void gc_add_mutator(gc_thread_data * thd)
{
  pthread_mutex_lock(&mutators_lock);
  if (ck_array_put_unique(&zyn_mutators, (void *)thd) < 0) {
    fprintf(stderr, "Unable to allocate memory for a new thread, exiting\n");
    exit(1);
  }
  ck_array_commit(&zyn_mutators);
  pthread_mutex_unlock(&mutators_lock);

  // Main thread is always the first one added
  if (primordial_thread == NULL) {
    primordial_thread = thd;
  } else {
    // At this point the mutator is running, so remove it from the new list
    pthread_mutex_lock(&mutators_lock);
    ck_array_remove(&new_mutators, (void *)thd);
    ck_array_commit(&new_mutators);
    pthread_mutex_unlock(&mutators_lock);
  }
}

/**
 * @brief Create a new heap page.
 *        The caller must hold the necessary locks.
 * @param  heap_type  Define the size of objects that will be allocated on this heap
 * @param  size       Requested size (unpadded) of the heap
 * @param  thd        Calling mutator's thread data object
 * @return Pointer to the newly allocated heap page, or NULL
 *         if the allocation failed.
 */
gc_heap *gc_heap_create(int heap_type, size_t size, gc_thread_data * thd)
{
  gc_free_list *free, *next;
  gc_heap *h;
  size_t padded_size;
  size = gc_heap_align(size);
  padded_size = gc_heap_pad_size(size);
  h = malloc(padded_size);
  if (!h)
    return NULL;
  h->type = heap_type;
  h->size = size;
  h->ttl = 10;
  h->next_free = h;
  h->last_alloc_size = 0;
  thd->cached_heap_total_sizes[heap_type] += size;
  thd->cached_heap_free_sizes[heap_type] += size;
  h->data = (char *)gc_heap_align(sizeof(h->data) + (uintptr_t) & (h->data));
  h->next = NULL;
  h->num_unswept_children = 0;
  free = h->free_list = (gc_free_list *) h->data;
  next = (gc_free_list *) (((char *)free) + gc_heap_align(gc_free_chunk_size));
  free->size = 0;               // First one is just a dummy record
  free->next = next;
  next->size = size - gc_heap_align(gc_free_chunk_size);
  next->next = NULL;
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG h->data addr: %p\n", &(h->data));
  fprintf(stderr, "DEBUG h->data addr: %p\n", h->data);
  fprintf(stderr, ("heap: %p-%p data: %p-%p size: %zu\n"),
          h, ((char *)h) + gc_heap_pad_size(size), h->data, h->data + size,
          size);
  fprintf(stderr, ("first: %p end: %p\n"), (object) gc_heap_first_block(h),
          (object) gc_heap_end(h));
  fprintf(stderr, ("free1: %p-%p free2: %p-%p\n"), free,
          ((char *)free) + free->size, next, ((char *)next) + next->size);
#endif
  if (heap_type <= LAST_FIXED_SIZE_HEAP_TYPE) {
    h->block_size = (heap_type + 1) * 32;
//
    h->remaining = size - (size % h->block_size);
    h->data_end = h->data + h->remaining;
    h->free_list = NULL;        // No free lists with bump&pop
// This is for starting with a free list, but we want bump&pop instead
//    h->remaining = 0;
//    h->data_end = NULL;
//    gc_init_fixed_size_free_list(h);
  } else {
    h->block_size = 0;
    h->remaining = 0;
    h->data_end = NULL;
  }
  // Lazy sweeping
  h->free_size = size;
  h->is_full = 0;
  h->is_unswept = 0;
  return h;
}

void gc_thread_data_init(gc_thread_data * thd, int mut_num, char *stack_base,
                         long stack_size)
{
  char stack_ref;
  thd->stack_start = stack_base;
#if STACK_GROWTH_IS_DOWNWARD
  thd->stack_limit = stack_base - stack_size;
#else
  thd->stack_limit = stack_base + stack_size;
#endif
  if (stack_overflow(stack_base, &stack_ref)) {
    fprintf(stderr,
            "Error: Stack is growing in the wrong direction! Rebuild with STACK_GROWTH_IS_DOWNWARD changed to %d\n",
            (1 - STACK_GROWTH_IS_DOWNWARD));
    exit(1);
  }
  thd->stack_traces = mi_calloc(MAX_STACK_TRACES, sizeof(char *));
  thd->stack_trace_idx = 0;
  thd->stack_prev_frame = NULL;
  thd->mutations = NULL;
  thd->mutation_buflen = 128;
  thd->mutation_count = 0;
  thd->mutations = vpbuffer_realloc(thd->mutations, &(thd->mutation_buflen));
  thd->globals_changed = 1;
  thd->param_objs = NULL;
  thd->exception_handler_stack = NULL;
  thd->scm_thread_obj = NULL;
  thd->thread_state = ZYN_THREAD_STATE_NEW;
  //thd->mutator_num = mut_num;
  thd->jmp_start = malloc(sizeof(jmp_buf));
  thd->gc_args = malloc(sizeof(object) * NUM_GC_ARGS);
  thd->gc_num_args = 0;
  thd->moveBufLen = 0;
  gc_thr_grow_move_buffer(thd);
  thd->gc_alloc_color = ck_pr_load_8(&gc_color_clear);
  thd->gc_trace_color = thd->gc_alloc_color;
  thd->gc_done_tracing = 0;
  thd->gc_status = ck_pr_load_int(&gc_status_col);
  thd->pending_writes = 0;
  thd->last_write = 0;
  thd->last_read = 0;
  thd->mark_buffer = mark_buffer_init(128);
  if (pthread_mutex_init(&(thd->lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize thread mutex\n");
    exit(1);
  }
  thd->heap_num_huge_allocations = 0;
  thd->num_minor_gcs = 0;
  thd->cached_heap_free_sizes = mi_calloc(5, sizeof(uintptr_t));
  thd->cached_heap_total_sizes = mi_calloc(5, sizeof(uintptr_t));
  thd->heap = mi_calloc(1, sizeof(gc_heap_root));
  thd->heap->heap = mi_calloc(1, sizeof(gc_heap *) * NUM_HEAP_TYPES);
  thd->heap->heap[HEAP_HUGE] = gc_heap_create(HEAP_HUGE, 1024, thd);
  for (int i = 0; i < HEAP_HUGE; i++) {
    thd->heap->heap[i] = gc_heap_create(i, INITIAL_HEAP_SIZE, thd);
  }
}

/**
 * @brief Increase the size of the mutator's move buffer
 * @param d Mutator's thread data object
 */
void gc_thr_grow_move_buffer(gc_thread_data * d)
{
  if (!d)
    return;

  if (d->moveBufLen == 0) {     // Special case
    d->moveBufLen = 128;
    d->moveBuf = NULL;
  } else {
    d->moveBufLen *= 2;
  }

  d->moveBuf = realloc(d->moveBuf, d->moveBufLen * sizeof(void *));
#if GC_DEBUG_TRACE
  fprintf(stderr, "grew moveBuffer, len = %d\n", d->moveBufLen);
#endif
}
