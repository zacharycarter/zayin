# Set the compiler and default flags.
CC ?= gcc
CFLAGS ?= -g # -O1

# SANITIZE variable controls the sanitizer mode:
# Set SANITIZE=asan for AddressSanitizer or SANITIZE=tsan for ThreadSanitizer.
SANITIZE ?=

# Set SAN_FLAGS based on the SANITIZE variable.
SAN_FLAGS :=
ifeq ($(SANITIZE),asan)
  SAN_FLAGS = -fsanitize=address
endif
ifeq ($(SANITIZE),tsan)
  SAN_FLAGS = -fsanitize=thread
endif

# Combine sanitizer flags with CFLAGS.
ALL_CFLAGS = $(CFLAGS) $(SAN_FLAGS)

TESTS = test_queue test_base

default: compiled_result

compiled_result: compiled_result.c gc.c base.c builtin.c bit_array.c scheduler.c thread_context.c
	$(CC) $(ALL_CFLAGS) compiled_result.c gc.c base.c builtin.c bit_array.c scheduler.c thread_context.c -o compiled_result -static-libasan

base.o: base.c base.h gc.h queue.h vec.h common.h
	$(CC) $(ALL_CFLAGS) -c base.c -o base.o

gc.o: gc.c gc.h base.h hash_table.h vec.h queue.h
	$(CC) $(ALL_CFLAGS) -c gc.c -o gc.o

builtin.o: builtin.c builtin.h base.h
	$(CC) $(ALL_CFLAGS) -c builtin.c -o builtin.o

bit_array.o: bit_array.c bit_array.h
	$(CC) $(ALL_CFLAGS) -c bit_array.c -o bit_array.o

test_queue: test_queue.c queue.h
	$(CC) $(ALL_CFLAGS) test_queue.c -o test_queue

test_base: test_base.c base.h base.o gc.o
	$(CC) $(ALL_CFLAGS) test_base.c base.o gc.o -o test_base

.PHONY: clean test

test: $(TESTS)
	@for t in $(TESTS); do \
		./$$t; \
	done

clean:
	-rm -f base.o
	-rm -f base
	-rm -f gc.o
	-rm -f builtin.o
	-rm -f compiled_result.o
	-rm -f bit_array.o
	-rm -f scheduler.o
	-rm -f thread_context.o
	-rm -f test_queue
	-rm -f test_base
	-rm -f compiled_result
