# Set the compiler and default flags.
CC ?= gcc
CFLAGS ?= -g # -O1

# Enable verbose output by default
V ?= 1

# Logging functions (macOS compatible)
ifeq ($(V),1)
  define log
    echo "🔷 $(1)"
  endef
  define log_cmd
    echo "🔶 $(1)" && $(1)
  endef
else
  define log
    @:
  endef
  define log_cmd
    @$(1)
  endef
endif

# Mimalloc paths (using relative paths)
MIMALLOC_DIR = thirdparty/mimalloc

# Set build directory and library name based on sanitizer mode
ifeq ($(SANITIZE),asan)
  MIMALLOC_BUILD_DIR = $(MIMALLOC_DIR)/out/asan
  MIMALLOC_STATIC_LIB = $(MIMALLOC_BUILD_DIR)/libmimalloc-asan.a
  CMAKE_EXTRA_FLAGS = -DMI_TRACK_ASAN=ON -DMI_OVERRIDE=OFF
else
  MIMALLOC_BUILD_DIR = $(MIMALLOC_DIR)/out/release
  MIMALLOC_STATIC_LIB = $(MIMALLOC_BUILD_DIR)/libmimalloc.a
  CMAKE_EXTRA_FLAGS = -DMI_OVERRIDE=OFF
endif

MIMALLOC_INCLUDE = $(MIMALLOC_DIR)/include

# SANITIZE variable controls the sanitizer mode
SANITIZE ?=

# Set SAN_FLAGS based on the SANITIZE variable.
SAN_FLAGS :=
ifeq ($(SANITIZE),asan)
  SAN_FLAGS = -fsanitize=address -fsanitize-recover=address
endif
ifeq ($(SANITIZE),tsan)
  SAN_FLAGS = -fsanitize=thread
endif

# Combine all flags in the correct order
ALL_CFLAGS = $(CFLAGS) -I$(MIMALLOC_INCLUDE) $(SAN_FLAGS)

TESTS = test_queue test_base

default: compiled_result

# Rule to clone and build mimalloc if needed
$(MIMALLOC_STATIC_LIB):
	$(call log,Setting up mimalloc (SANITIZE=$(SANITIZE)))
	$(call log,Checking if mimalloc needs to be cloned...)
	@if [ ! -d "$(MIMALLOC_DIR)" ]; then \
		$(call log,Cloning mimalloc...); \
		mkdir -p thirdparty && \
		git clone https://github.com/microsoft/mimalloc.git $(MIMALLOC_DIR); \
	fi
	$(call log,Creating build directory $(MIMALLOC_BUILD_DIR))
	@mkdir -p $(MIMALLOC_BUILD_DIR)
	$(call log,Configuring mimalloc with cmake...)
	@cd $(MIMALLOC_BUILD_DIR) && \
	cmake $(CURDIR)/$(MIMALLOC_DIR) -DCMAKE_BUILD_TYPE=Release \
		$(CMAKE_EXTRA_FLAGS) \
		-DMI_BUILD_SHARED=OFF \
		-DMI_BUILD_TESTS=OFF
	$(call log,Building mimalloc...)
	@cd $(MIMALLOC_BUILD_DIR) && make
	$(call log,Mimalloc built successfully ✅)

# Updated compilation rules to include mimalloc
# compiled_result: compiled_result.c gc.c base.c builtin.c bit_array.c scheduler.c thread_context.c $(MIMALLOC_STATIC_LIB)
compiled_result: compiled_result.c gc.c base.c builtin.c bit_array.c $(MIMALLOC_STATIC_LIB)
	$(call log,Building compiled_result)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) $^ -o $@)

base.o: base.c base.h gc.h queue.h vec.h common.h $(MIMALLOC_STATIC_LIB)
	$(call log,Building base.o)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) -c base.c -o base.o)

gc.o: gc.c gc.h base.h hash_table.h vec.h queue.h $(MIMALLOC_STATIC_LIB)
	$(call log,Building gc.o)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) -c gc.c -o gc.o)

builtin.o: builtin.c builtin.h base.h $(MIMALLOC_STATIC_LIB)
	$(call log,Building builtin.o)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) -c builtin.c -o builtin.o)

bit_array.o: bit_array.c bit_array.h $(MIMALLOC_STATIC_LIB)
	$(call log,Building bit_array.o)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) -c bit_array.c -o bit_array.o)

test_queue: test_queue.c queue.h $(MIMALLOC_STATIC_LIB)
	$(call log,Building test_queue)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) test_queue.c $(MIMALLOC_STATIC_LIB) -o test_queue)

test_base: test_base.c base.h base.o gc.o $(MIMALLOC_STATIC_LIB)
	$(call log,Building test_base)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) test_base.c base.o gc.o $(MIMALLOC_STATIC_LIB) -o test_base)

.PHONY: clean test clean-mimalloc clean-all build-asan show-flags

# Add a convenience target to show flags
show-flags:
	$(call log,Compiler: $(CC))
	$(call log,CFLAGS: $(CFLAGS))
	$(call log,ALL_CFLAGS: $(ALL_CFLAGS))
	$(call log,SANITIZE: $(SANITIZE))
	$(call log,SAN_FLAGS: $(SAN_FLAGS))
	$(call log,MIMALLOC_DIR: $(MIMALLOC_DIR))
	$(call log,MIMALLOC_BUILD_DIR: $(MIMALLOC_BUILD_DIR))
	$(call log,MIMALLOC_STATIC_LIB: $(MIMALLOC_STATIC_LIB))

# Add a convenience target for ASAN builds
build-asan:
	$(call log,Starting ASAN build)
	$(MAKE) clean-all
	$(MAKE) SANITIZE=asan

test: $(TESTS)
	$(call log,Running tests with ASAN_OPTIONS=verbosity=1)
	@for t in $(TESTS); do \
		$(call log,Running $$t); \
		ASAN_OPTIONS=verbosity=1 ./$$t; \
	done

clean:
	$(call log,Cleaning build artifacts)
	-rm -f base.o
	-rm -f base
	-rm -f gc.o
	-rm -f builtin.o
	-rm -f compiled_result.o
	-rm -f bit_array.o
	# -rm -f scheduler.o
	# -rm -f thread_context.o
	-rm -f test_queue
	-rm -f test_base
	-rm -f compiled_result

clean-mimalloc:
	$(call log,Cleaning mimalloc build directory)
	-rm -rf $(MIMALLOC_DIR)/out

clean-all: clean clean-mimalloc
	$(call log,Cleaning all artifacts including mimalloc repository)
	-rm -rf $(MIMALLOC_DIR)
