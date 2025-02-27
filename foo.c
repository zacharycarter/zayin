#include <mimalloc.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "base.h"
#include "builtin.h"
#include "runtime_main.h"
struct env_0 {
struct obj *v_k_7;
struct obj *v_rv_15;
};

void lambda_0(struct obj *var_0,struct env_obj *env_in) __attribute__((noreturn));

struct env_1 {
struct obj *v_k_7;
struct obj *v_rv_15;
};

void lambda_1(struct obj *var_4,struct obj *var_5,struct env_obj *env_in) __attribute__((noreturn));

struct env_2 {
struct obj *v_k_3;
struct obj *v_k_7;
struct obj *v_rv_15;
};

void lambda_2(struct obj *var_12,struct obj *var_13,struct env_obj *env_in) __attribute__((noreturn));

void lambda_0(struct obj *var_0,struct env_obj *env_in) {
struct env_0 *var_1 = (struct env_0*)(&((env_in)->env));
OBJECT_CELL_OBJ_NEW(var_2,var_0);
((var_1)->v_rv_15)=(var_2);
OBJECT_INT_OBJ_NEW(var_3,1);
call_closure_two(((struct cell_obj*)((var_1)->v_rv_15))->val,var_3,((struct cell_obj*)((var_1)->v_k_7))->val);
__builtin_unreachable();
}

void lambda_1(struct obj *var_4,struct obj *var_5,struct env_obj *env_in) {
struct env_1 *var_6 = (struct env_1*)(&((env_in)->env));
OBJECT_CELL_OBJ_NEW(var_7,var_5);
((var_6)->v_k_7)=(var_7);
OBJECT_CLOSURE_TWO_NEW(var_8,add_k,NULL);
OBJECT_INT_OBJ_NEW(var_9,1);
OBJECT_ENV_OBJ_NEW(var_10,struct env_0);
(((struct env_0*)(&((var_10)->env)))->v_k_7)=((var_6)->v_k_7);
(((struct env_0*)(&((var_10)->env)))->v_rv_15)=((var_6)->v_rv_15);
OBJECT_CLOSURE_ONE_NEW(var_11,lambda_0,var_10);
call_closure_two(var_8,var_9,var_11);
__builtin_unreachable();
}

void lambda_2(struct obj *var_12,struct obj *var_13,struct env_obj *env_in) {
struct env_2 *var_14 = (struct env_2*)(&((env_in)->env));
OBJECT_CELL_OBJ_NEW(var_15,var_13);
((var_14)->v_k_3)=(var_15);
OBJECT_ENV_OBJ_NEW(var_16,struct env_1);
(((struct env_1*)(&((var_16)->env)))->v_k_7)=((var_14)->v_k_7);
(((struct env_1*)(&((var_16)->env)))->v_rv_15)=((var_14)->v_rv_15);
OBJECT_CLOSURE_TWO_NEW(var_17,lambda_1,var_16);
call_closure_two(var_17,NULL,((struct cell_obj*)((var_14)->v_k_3))->val);
__builtin_unreachable();
}

void main_lambda(struct obj *input_obj,struct env_obj *env) {
OBJECT_ENV_OBJ_NEW(var_18,struct env_2);
OBJECT_CLOSURE_TWO_NEW(var_19,lambda_2,var_18);
OBJECT_CLOSURE_ONE_NEW(var_20,exit_k,NULL);
call_closure_two(var_19,NULL,var_20);
__builtin_unreachable();
}
int main(void) {
    mi_version();
    struct closure_obj initial_closure = object_closure_one_new(main_lambda, NULL);
    struct thunk initial_thunk = {
        .closr = &initial_closure,
        .one = {NULL},
    };

    struct thunk *thnk_heap = mi_malloc(sizeof(struct thunk));
    memcpy(thnk_heap, &initial_thunk, sizeof(struct thunk));
    zayin_start(thnk_heap);
}
