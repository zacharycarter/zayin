/**
 * Adaptation of the simple addition example to use Cyclone Scheme runtime
 */

#define closcall1(td, clo, buf) \
if (obj_is_not_closure(clo)) { \
   zyn_apply(td, clo, 1, buf ); \
} else { \
   ((clo)->fn)(td, clo, 1, buf); \
;\
}
#define return_closcall1(td, clo,a1) { \
 char top; \
 object buf[1]; buf[0] = a1;\
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     GC(td, clo, buf, 1); \
     return; \
 } else {\
     closcall1(td, (closure) (clo), buf); \
     return;\
 } \
}

#define closcall2(td, clo, buf) \
if (obj_is_not_closure(clo)) { \
   zyn_apply(td, clo, 2, buf ); \
} else { \
   ((clo)->fn)(td, clo, 2, buf); \
;\
}
#define return_closcall2(td, clo,a1,a2) { \
 char top; \
 object buf[2]; buf[0] = a1;buf[1] = a2;\
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     GC(td, clo, buf, 2); \
     return; \
 } else {\
     closcall2(td, (closure) (clo), buf); \
     return;\
 } \
}

#include <mimalloc.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "base.h"
#include "runtime_main.h"

/* Environment structure for the first lambda */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object k_7;
  object rv_15;
} env_0_type;

/* Environment structure for the second lambda */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object k_7;
  object rv_15;
} env_1_type;

/* Environment structure for the third lambda */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object k_3;
  object k_7;
  object rv_15;
} env_2_type;

/* Forward declarations of all lambda functions */
static void lambda_0(void *data, object clo, int argc, object *args);
static void lambda_1(void *data, object clo, int argc, object *args);
static void lambda_2(void *data, object clo, int argc, object *args);
static void main_lambda(void *data, object clo, int argc, object *args);

/* Lambda 0 - Final processing step that receives the result */
static void lambda_0(void *data, object clo, int argc, object *args) {
  object var_0 = args[0];

  /* Access our environment */
  env_0_type *env = (env_0_type *)clo;

  /* Create a cell containing our value */
  make_cell(var_2, var_0);

  /* Store it in the environment */
  env->rv_15 = &var_2;

  /* Create the integer 1 */
  object var_3 = obj_int2obj(1);

  /* Call the continuation with our value and the next continuation */
  return_closcall2(data, cell_get(env->rv_15), var_3, cell_get(env->k_7));
}

/* Lambda 1 - Sets up environment and calculates 1+1 */
static void lambda_1(void *data, object clo, int argc, object *args) {
  object var_4 = args[0];
  object var_5 = args[1];

  /* Access our environment */
  env_1_type *env = (env_1_type *)clo;

  /* Create a cell containing our continuation */
  make_cell(var_7, var_5);

  /* Store it in environment */
  env->k_7 = &var_7;

  /* Create integer 1 for addition */
  object var_9 = obj_int2obj(1);

  /* Create new environment for lambda_0 */
  env_0_type *new_env = alloca(sizeof(env_0_type));
  new_env->hdr.mark = gc_color_red;
  new_env->hdr.grayed = 0;
  new_env->tag = closureN_tag;
  new_env->k_7 = env->k_7;
  new_env->rv_15 = env->rv_15;

  /* Create closure for lambda_0 */
  closure1_type var_11;
  var_11.hdr.mark = gc_color_red;
  var_11.hdr.grayed = 0;
  var_11.tag = closure1_tag;
  var_11.fn = lambda_0;
  var_11.num_args = 1;
  var_11.element = (object)new_env;

  /* Calculate 1+1 using Cyclone's addition function */
  complex_num_type local_result;
  object result = zyn_fast_sum(data, &local_result, var_9, var_9);
  return_closcall1(data, &var_11, result);
}

/* Lambda 2 - Sets up initial environment and calls lambda_1 */
static void lambda_2(void *data, object clo, int argc, object *args) {
  object var_12 = args[0];
  object var_13 = args[1];

  /* Access our environment */
  env_2_type *env = (env_2_type *)clo;

  /* Create a cell containing our continuation */
  make_cell(var_15, var_13);

  /* Store it in environment */
  env->k_3 = &var_15;

  /* Create new environment for lambda_1 */
  env_1_type *new_env = alloca(sizeof(env_1_type));
  new_env->hdr.mark = gc_color_red;
  new_env->hdr.grayed = 0;
  new_env->tag = closureN_tag;
  new_env->k_7 = env->k_7;
  new_env->rv_15 = env->rv_15;

  /* Create closure for lambda_1 */
  closure1_type var_17;
  var_17.hdr.mark = gc_color_red;
  var_17.hdr.grayed = 0;
  var_17.tag = closure1_tag;
  var_17.fn = lambda_1;
  var_17.num_args = 2;
  var_17.element = (object)new_env;

  /* Call with lambda_1 passing NULL and our continuation */
  return_closcall2(data, &var_17, NULL, cell_get(env->k_3));
}

/* Initial lambda to kick off the computation */
static void main_lambda(void *data, object clo, int argc, object *args) {
  /* Create environment for lambda_2 */
  env_2_type *env = alloca(sizeof(env_2_type));
  make_c_opaque(var_18, env)
  var_18->hdr.mark = gc_color_red;
  var_18->hdr.grayed = 0;
  var_18->tag = closureN_tag;

  /* Create closure for lambda_2 */
  closure1_type var_19;
  var_19.hdr.mark = gc_color_red;
  var_19.hdr.grayed = 0;
  var_19.tag = closure1_tag;
  var_19.fn = lambda_2;
  var_19.num_args = 2;
  var_19.element = (object)var_18;

  /* Create the exit continuation using Cyclone's __halt */
  mclosure0(exit_clo, (function_type)__halt);

  /* Start the computation */
  return_closcall2(data, &var_19, NULL, &exit_clo);
}

/* Entry point setup for Cyclone runtime */
static void c_entry_pt(void *data, object clo, int argc, object *args) {
  mclosure0(entry_clo, (function_type)main_lambda);
  return_closcall1(data, args[0], &entry_clo);
}

/* Standard Cyclone main entry point */
int main(int argc, char **argv, char **envp) {
  gc_thread_data *thd;
  long stack_size = global_stack_size = STACK_SIZE;
  long heap_size = global_heap_size = HEAP_SIZE;

  /* Initialize Cyclone runtime */
  mclosure0(clos_halt, &zyn_exit);
  mclosure0(entry_pt, &c_entry_pt);
  _cyc_argc = argc;
  _cyc_argv = argv;
  set_env_variables(envp);
  gc_initialize();

  /* Set up thread data */
  thd = malloc(sizeof(gc_thread_data));
  gc_thread_data_init(thd, 0, (char *) &stack_size, stack_size);
  thd->gc_cont = &entry_pt;
  thd->gc_args[0] = &clos_halt;
  thd->gc_num_args = 1;
  thd->thread_id = pthread_self();
  gc_add_mutator(thd);

  /* Initialize heap and start execution */
  zyn_heap_init(heap_size);
  thd->thread_state = ZYN_THREAD_STATE_RUNNABLE;
  zyn_start_trampoline(thd);
  return 0;
}
