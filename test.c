/**
 * Adaptation of zayin_compiled_example.c to use Cyclone Scheme runtime
 */

#define closcall1(td, clo, buf) \
if (obj_is_not_closure(clo)) { \
   Cyc_apply(td, clo, 1, buf ); \
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
   Cyc_apply(td, clo, 2, buf ); \
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

#include "cyclone/types.h"

extern object __glo_display_scheme_write;

#include "cyclone/runtime.h"
#include "cyclone/runtime-main.h"

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
  object rv_31;
} env_1_type;

/* Environment structure for the third lambda */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object k_7;
  object rv_15;
  object rv_31;
} env_2_type;

/* Environment structure for the fourth lambda */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object k_3;
  object k_7;
  object rv_15;
  object rv_31;
} env_3_type;

/* Forward declarations of all lambda functions */
static void lambda_0(void *data, object clo, int argc, object *args);
static void lambda_1(void *data, object clo, int argc, object *args);
static void lambda_2(void *data, object clo, int argc, object *args);
static void lambda_3(void *data, object clo, int argc, object *args);
static void main_lambda(void *data, object clo, int argc, object *args);

/* Lambda 0 - Displays the result and passes to final continuation */
static void lambda_0(void *data, object clo, int argc, object *args) {
  object var_0 = args[0];

  /* Access our environment */
  env_0_type *env = (env_0_type *)clo;

  /* Create a cell containing our value */
  make_cell(var_2, var_0);

  /* Store it in the environment */
  env->rv_15 = &var_2;

  /* Call display with our value and the continuation */
  object display_fn = __glo_display_scheme_write;
  return_closcall2(data, display_fn, cell_get(env->rv_15), env->k_7);
}

/* Lambda 1 - Creates cell for our value and calls lambda_0 */
static void lambda_1(void *data, object clo, int argc, object *args) {
  object var_4 = args[0];

  /* Access our environment */
  env_1_type *env = (env_1_type *)clo;

  /* Create a cell containing our value */
  make_cell(var_6, var_4);

  /* Store it in environment */
  env->rv_31 = &var_6;

  /* Create the integer 1 */
  object var_7 = obj_int2obj(1);

  /* Create new environment for lambda_0 */
  env_0_type *new_env = alloca(sizeof(env_0_type));
  new_env->hdr.mark = gc_color_red;
  new_env->hdr.grayed = 0;
  new_env->tag = closureN_tag;
  new_env->k_7 = env->k_7;
  new_env->rv_15 = env->rv_15;

  /* Create closure for lambda_0 */
  closure1_type var_9;
  var_9.hdr.mark = gc_color_red;
  var_9.hdr.grayed = 0;
  var_9.tag = closure1_tag;
  var_9.fn = lambda_0;
  var_9.num_args = 1;
  var_9.element = (object)new_env;

  /* Call our continuation with the value and next function */
  return_closcall2(data, cell_get(env->rv_31), var_7, &var_9);
}

/* Lambda 2 - Sets up continuation and calls add */
static void lambda_2(void *data, object clo, int argc, object *args) {
  object var_10 = args[0];
  object var_11 = args[1];

  /* Access our environment */
  env_2_type *env = (env_2_type *)clo;

  /* Create a cell containing our continuation */
  make_cell(var_13, var_11);

  /* Store it in environment */
  env->k_7 = &var_13;

  /* Create integer 1 for addition */
  object var_15 = obj_int2obj(1);

  /* Create new environment for lambda_1 */
  env_1_type *new_env = alloca(sizeof(env_1_type));
  new_env->hdr.mark = gc_color_red;
  new_env->hdr.grayed = 0;
  new_env->tag = closureN_tag;
  new_env->k_7 = env->k_7;
  new_env->rv_15 = env->rv_15;
  new_env->rv_31 = env->rv_31;

  /* Create closure for lambda_1 */
  closure1_type var_17;
  var_17.hdr.mark = gc_color_red;
  var_17.hdr.grayed = 0;
  var_17.tag = closure1_tag;
  var_17.fn = lambda_1;
  var_17.num_args = 1;
  var_17.element = (object)new_env;

  /* Calculate 1+1 using Cyclone's addition function and pass to lambda_1 */
  complex_num_type local_result;
  object result = Cyc_fast_sum(data, &local_result, var_15, var_15);
  return_closcall1(data, &var_17, result);
}

/* Lambda 3 - Sets up initial environment and calls lambda_2 */
static void lambda_3(void *data, object clo, int argc, object *args) {
  object var_18 = args[0];
  object var_19 = args[1];

  /* Access our environment */
  env_3_type *env = (env_3_type *)clo;

  /* Create a cell containing our continuation */
  make_cell(var_21, var_19);

  /* Store it in environment */
  env->k_3 = &var_21;

  /* Create new environment for lambda_2 */
  env_2_type *new_env = alloca(sizeof(env_2_type));
  new_env->hdr.mark = gc_color_red;
  new_env->hdr.grayed = 0;
  new_env->tag = closureN_tag;
  new_env->k_7 = env->k_7;
  new_env->rv_15 = env->rv_15;
  new_env->rv_31 = env->rv_31;

  /* Create closure for lambda_2 */
  closure1_type var_23;
  var_23.hdr.mark = gc_color_red;
  var_23.hdr.grayed = 0;
  var_23.tag = closure1_tag;
  var_23.fn = lambda_2;
  var_23.num_args = 2;
  var_23.element = (object)new_env;

  /* Call with lambda_2 passing NULL and our continuation */
  return_closcall2(data, &var_23, NULL, cell_get(env->k_3));
}

/* Initial lambda to kick off the computation */
static void main_lambda(void *data, object clo, int argc, object *args) {
  /* Create environment for lambda_3 */
  env_3_type *var_24 = alloca(sizeof(env_3_type));
  var_24->hdr.mark = gc_color_red;
  var_24->hdr.grayed = 0;
  var_24->tag = closureN_tag;

  /* Create closure for lambda_3 */
  closure1_type var_25;
  var_25.hdr.mark = gc_color_red;
  var_25.hdr.grayed = 0;
  var_25.tag = closure1_tag;
  var_25.fn = lambda_3;
  var_25.num_args = 2;
  var_25.element = (object)var_24;

  /* Create the exit continuation using Cyclone's __halt */
  mclosure0(halt_clo, (function_type)__halt);

  /* Start the computation */
  return_closcall2(data, &var_25, NULL, &halt_clo);
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
  mclosure0(clos_halt, &Cyc_halt);
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
  Cyc_heap_init(heap_size);
  thd->thread_state = CYC_THREAD_STATE_RUNNABLE;
  Cyc_start_trampoline(thd);
  return 0;
}
