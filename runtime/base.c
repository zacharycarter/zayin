#include <mimalloc.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>

#include "base.h"
#include "common.h"
#include "gc.h"
#include "hash_table.h"
#include "scheduler.h"
#include "thread_context.h"
#include "vec.h"

/* Forward declarations */
static bool stack_check(void);

/* Global variables */
/* For backwards compatibility - used by single-threaded mode */
void *global_stack_initial = NULL;
struct thunk *current_thunk = NULL;
jmp_buf setjmp_env_buf;

/* Modified to use thread context */
void call_closure_one(struct obj *rator, struct obj *rand) {
  struct closure_obj *closure;
  thread_context_t *ctx;
  struct thunk *thnk_heap;

  if (rator->tag != OBJ_CLOSURE) {
    RUNTIME_ERROR("Called object (%p) was not a closure but was: %d", rator,
                  rator->tag);
  }

  closure = (struct closure_obj *)rator;

  if (closure->size != CLOSURE_ONE) {
    printf("Trying to call: %p\n", closure->fn_1);
    RUNTIME_ERROR("Called a closure that takes two args with one arg");
  }

  ctx = get_current_thread_context();

  if (stack_check()) {
    closure->fn_1(rand, closure->env);
  } else {
    /* Allocate thunk for the current thread */
    thnk_heap = gc_malloc(sizeof(struct thunk));
    thnk_heap->closr = closure;
    thnk_heap->one.rand = rand;

    /* Run minor GC for the current thread */
    if (ctx && ctx->gc_state.gc_ctx) {
      gc_minor(ctx->gc_state.gc_ctx, thnk_heap);
    } else {
      /* Fallback to old behavior if thread context not set up */
      run_minor_gc(thnk_heap);
    }
  }
}

/* Modified to use thread context */
void call_closure_two(struct obj *rator, struct obj *rand, struct obj *cont) {
  struct closure_obj *closure;
  thread_context_t *ctx;
  struct thunk *thnk_heap;

  if (rator->tag != OBJ_CLOSURE) {
    RUNTIME_ERROR("Called object (%p) was not a closure but was: %d", rator,
                  rator->tag);
  }

  closure = (struct closure_obj *)rator;

  if (closure->size != CLOSURE_TWO) {
    RUNTIME_ERROR("Called a closure that takes one arg with two args");
  }

  ctx = get_current_thread_context();

  if (stack_check()) {
    closure->fn_2(rand, cont, closure->env);
  } else {
    /* Allocate thunk for the current thread */
    thnk_heap = gc_malloc(sizeof(struct thunk));
    thnk_heap->closr = closure;
    thnk_heap->two.rand = rand;
    thnk_heap->two.cont = cont;

    /* Run minor GC for the current thread */
    if (ctx && ctx->gc_state.gc_ctx) {
      gc_minor(ctx->gc_state.gc_ctx, thnk_heap);
    } else {
      /* Fallback to old behavior if thread context not set up */
      run_minor_gc(thnk_heap);
    }
  }
}

static size_t get_stack_limit(void) {
  static size_t cached_limit = 0;

  if (cached_limit != 0) {
    return cached_limit;
  }

  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  cached_limit = limit.rlim_cur;
  return cached_limit;
}

void *stack_ptr(void) {
  return __builtin_frame_address(0);
}

/*
 * Are we above the stack limit
 */
static bool stack_check(void) {
  /* buffer area at the end of the stack since idk how accurate this is
   * so reserve 256K for anything we might do after getting to the 'limit' */
  static size_t stack_buffer = 1024 * 256;
  uintptr_t stack_ptr_val;
  uintptr_t stack_end_val;
  void *stack_initial;

  thread_context_t *ctx = get_current_thread_context();
  stack_initial = ctx ? ctx->stack_initial : NULL;

  if (!stack_initial) {
    /* Fall back to single-threaded global if thread context not available */
    stack_initial = global_stack_initial;
  }

  stack_ptr_val = (uintptr_t)stack_ptr();
  stack_end_val =
      (uintptr_t)(stack_initial - get_stack_limit() + stack_buffer);

  return stack_ptr_val > stack_end_val;
}

void zayin_start(struct thunk *initial_thunk) {
  thread_context_t *ctx;
  struct closure_obj *closr;
  struct obj *rand;
  struct obj *cont;
  struct env_obj *env;
  struct thunk *thnk;
  static bool gc_initialized = false;

  ctx = get_current_thread_context();

  /* Initialize stack pointer */
  if (ctx) {
    ctx->stack_initial = stack_ptr();
    ctx->current_thunk = initial_thunk;
  } else {
    /* Fallback for compatibility with single-threaded code */
    global_stack_initial = stack_ptr();
  }

  /* Initialize GC if needed */
  if (!gc_initialized) {
    gc_global_init();
    gc_initialized = true;
  }

  /* Initialize thread-local GC if this is a thread context */
  if (ctx && !ctx->gc_state.nursery) {
    gc_thread_init(ctx->thread_index);
  }

  /* Set up local variables for the current thunk */
  if (ctx) {
    /* Store jump buffer in thread context */
    if (setjmp(ctx->setjmp_env) == 0) {
      /* First time through */
      DEBUG_FPRINTF(stderr, "Starting execution in thread %d\n", ctx->thread_index);

      if (ctx->current_thunk->closr->size == CLOSURE_ONE) {
        closr = ctx->current_thunk->closr;
        rand = ctx->current_thunk->one.rand;
        env = ctx->current_thunk->closr->env;
        thnk = ctx->current_thunk;

        /* Clear current thunk reference before calling to avoid double-free */
        ctx->current_thunk = NULL;

        /* Free the thunk */
        mi_free(thnk);

        /* Call the closure */
        closr->fn_1(rand, env);
      } else {
        closr = ctx->current_thunk->closr;
        rand = ctx->current_thunk->two.rand;
        cont = ctx->current_thunk->two.cont;
        env = ctx->current_thunk->closr->env;
        thnk = ctx->current_thunk;

        /* Clear current thunk reference before calling to avoid double-free */
        ctx->current_thunk = NULL;

        /* Free the thunk */
        mi_free(thnk);

        /* Call the closure */
        closr->fn_2(rand, cont, env);
      }
    } else {
      /* Coming back from longjmp - bounce to new thunk */
      DEBUG_FPRINTF(stderr, "Bouncing to new thunk in thread %d\n", ctx->thread_index);

      if (ctx->current_thunk->closr->size == CLOSURE_ONE) {
        closr = ctx->current_thunk->closr;
        rand = ctx->current_thunk->one.rand;
        env = ctx->current_thunk->closr->env;
        thnk = ctx->current_thunk;

        /* Clear current thunk reference before calling to avoid double-free */
        ctx->current_thunk = NULL;

        /* Free the thunk */
        mi_free(thnk);

        /* Call the closure */
        closr->fn_1(rand, env);
      } else {
        closr = ctx->current_thunk->closr;
        rand = ctx->current_thunk->two.rand;
        cont = ctx->current_thunk->two.cont;
        env = ctx->current_thunk->closr->env;
        thnk = ctx->current_thunk;

        /* Clear current thunk reference before calling to avoid double-free */
        ctx->current_thunk = NULL;

        /* Free the thunk */
        mi_free(thnk);

        /* Call the closure */
        closr->fn_2(rand, cont, env);
      }
    }
  } else {
    /* Fallback for compatibility with single-threaded code */
    /* This is the old trampoline code */
    current_thunk = initial_thunk;

    /* This is our trampoline, when we come back from a longjmp a different
       current_thunk will be set and we will just trampoline into the new
       thunk */
    if (setjmp(setjmp_env_buf) == 0) {
      /* First time through */
      DEBUG_FPRINTF(stderr, "Starting execution (single-threaded mode)\n");
    } else {
      /* Coming back from longjmp */
      DEBUG_FPRINTF(stderr, "bouncing\n");
    }

    if (current_thunk->closr->size == CLOSURE_ONE) {
      closr = current_thunk->closr;
      rand = current_thunk->one.rand;
      env = current_thunk->closr->env;
      mi_free(current_thunk);
      closr->fn_1(rand, env);
    } else {
      closr = current_thunk->closr;
      rand = current_thunk->two.rand;
      cont = current_thunk->two.cont;
      env = current_thunk->closr->env;
      mi_free(current_thunk);
      closr->fn_2(rand, cont, env);
    }
  }

  RUNTIME_ERROR("Control flow returned from trampoline function.");
}

/* Updated to use thread context */
void run_minor_gc(struct thunk *thnk) {
  thread_context_t *ctx;
  struct gc_context *gc_ctx;

  ctx = get_current_thread_context();

  if (ctx) {
    /* Use thread-local context */
    ctx->current_thunk = thnk;

    if (ctx->gc_state.gc_ctx) {
      /* Use thread-local GC context */
      gc_minor(ctx->gc_state.gc_ctx, thnk);
    } else {
      /* Fallback to old behavior */
      gc_ctx = gc_make_context();
      gc_minor(gc_ctx, thnk);
      gc_free_context(gc_ctx);
    }

    /* Jump back to the trampoline */
    longjmp(ctx->setjmp_env, 1);
  } else {
    /* Fallback for compatibility with single-threaded code */
    current_thunk = thnk;

    gc_ctx = gc_make_context();
    gc_minor(gc_ctx, thnk);
    gc_free_context(gc_ctx);

    /* Jump back to the start */
    longjmp(setjmp_env_buf, 1);
  }
}

/* The rest of the code is unchanged */
struct obj object_base_new(enum object_tag tag) {
  return (struct obj){
      .tag = tag,
      .mark = WHITE,
      .on_stack = true,
#ifdef DEBUG
      .last_touched_by = "object_init",
#endif
  };
}

struct closure_obj object_closure_one_new(void (*fn)(struct obj *,
                                                     struct env_obj *),
                                          struct env_obj *env) {
  return (struct closure_obj){.base = object_base_new(OBJ_CLOSURE),
                              .size = CLOSURE_ONE,
                              .fn_1 = fn,
                              .env = env};
}

struct closure_obj object_closure_two_new(void (*fn)(struct obj *, struct obj *,
                                                     struct env_obj *),
                                          struct env_obj *env) {
  return (struct closure_obj){.base = object_base_new(OBJ_CLOSURE),
                              .size = CLOSURE_TWO,
                              .fn_2 = fn,
                              .env = env};
}

struct int_obj object_int_obj_new(int64_t val) {
  return (struct int_obj){.base = object_base_new(OBJ_INT), .val = val};
}

struct bool_obj object_bool_obj_new(bool val) {
  return (struct bool_obj){.base = object_base_new(OBJ_BOOL), .val = val};
}

struct cons_obj object_cons_obj_new(struct obj *car, struct obj *cdr) {
  return (struct cons_obj){
      .base = object_base_new(OBJ_CONS), .car = car, .cdr = cdr};
}

static size_t hash_string(const char *buf, size_t len) {
  size_t hash = 14695981039346656037ull;
  size_t i;

  for (i = 0; i < len; i++) {
    hash ^= (size_t)buf[i];
    hash *= 1099511628211;
  }

  return hash;
}

static size_t hash_combine(size_t a, size_t b) {
  size_t hash = a;

  hash *= 1099511628211;
  hash ^= b;
  hash *= 1099511628211;

  return hash;
}

size_t hash_obj_impl(struct obj *obj) {
  struct string_obj *str_obj;
  struct cons_obj *cons_obj;
  struct ht_obj *ht_obj;
  size_t a, b, hash;

  if (!obj) {
    return 0;
  }

  switch (obj->tag) {
  case OBJ_INT:
    return hash_table_default_size_t_hash_fun(((struct int_obj *)obj)->val);
  case OBJ_BOOL:
    return hash_table_default_size_t_hash_fun(((struct bool_obj *)obj)->val);
  case OBJ_STR:
    str_obj = (struct string_obj *)obj;
    return hash_string(str_obj->buf, str_obj->len);
  case OBJ_CONS:
    cons_obj = (struct cons_obj *)obj;
    a = hash_obj_impl(cons_obj->car);
    b = hash_obj_impl(cons_obj->cdr);
    return hash_combine(a, b);
  case OBJ_HT:
    ht_obj = (struct ht_obj *)obj;
    hash = 14695981039346656037ull;

    HASH_TABLE_ITER(obj, key, val, ht_obj->ht, {
      hash = hash_combine(hash, hash_obj_impl(*key));
      hash = hash_combine(hash, hash_obj_impl(*val));
    });

    return hash;
  case OBJ_CELL:
    return hash_obj_impl(((struct cell_obj *)obj)->val);
  default:
    RUNTIME_ERROR("Unhashable type: %d", obj->tag);
  }
}

bool eq_obj_impl(struct obj *a, struct obj *b) {
  struct string_obj *str_obj_a, *str_obj_b;
  struct cons_obj *cons_obj_a, *cons_obj_b;
  struct ht_obj *ht_obj_a, *ht_obj_b;

  if (!a && !b)
    return true;

  if (!a || !b)
    return false;

  if (a->tag != b->tag)
    return false;

  switch (a->tag) {
  case OBJ_INT:
    return ((struct int_obj *)a)->val == ((struct int_obj *)b)->val;
  case OBJ_BOOL:
    return ((struct bool_obj *)a)->val == ((struct bool_obj *)b)->val;
  case OBJ_STR:
    str_obj_a = (struct string_obj *)a;
    str_obj_b = (struct string_obj *)b;

    if (str_obj_a->len != str_obj_b->len)
      return false;

    return strncmp(str_obj_a->buf, str_obj_b->buf, str_obj_a->len) == 0;
  case OBJ_CONS:
    cons_obj_a = (struct cons_obj *)a;
    cons_obj_b = (struct cons_obj *)b;
    return eq_obj_impl(cons_obj_a->car, cons_obj_b->car) &&
           eq_obj_impl(cons_obj_a->cdr, cons_obj_b->cdr);
  case OBJ_HT:
    ht_obj_a = (struct ht_obj *)a;
    ht_obj_b = (struct ht_obj *)b;

    HASH_TABLE_ITER(obj, key, val, ht_obj_a->ht, {
      typeof(*val) *b_val = hash_table_obj_lookup(ht_obj_b->ht, *key);

      if (!b_val)
        return false;

      if (!eq_obj_impl(*val, *b_val))
        return false;
    });

    HASH_TABLE_ITER(obj, key, val, ht_obj_b->ht, {
      typeof(*val) *a_val = hash_table_obj_lookup(ht_obj_a->ht, *key);

      if (!a_val)
        return false;

      /* we don't need to check equality of values this time */
    });

    return true;
  case OBJ_CELL:
    return eq_obj_impl(((struct cell_obj *)a)->val,
                       ((struct cell_obj *)b)->val);
  default:
    return false;
  }
}

MAKE_HASH(struct obj *, struct obj *, hash_obj_impl, eq_obj_impl, obj);

struct ht_obj object_ht_obj_new() {
  struct hash_table_obj *ht = hash_table_obj_new();

  return (struct ht_obj){.base = object_base_new(OBJ_HT), .ht = ht};
}
