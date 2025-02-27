# Set the compiler and default flags.
CC ?= gcc
CFLAGS ?= -g -O1

# Enable verbose output by default
V ?= 1

# Add parallelism to make, but disable for better error messages
MAKEFLAGS += --no-print-directory

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

# Common directory for shared dependencies across projects
# Use absolute path for consistent access from any project folder
DEPS_ROOT ?= $(HOME)/.cache/compiler_deps

# Mimalloc paths (using shared location)
MIMALLOC_DIR = $(DEPS_ROOT)/mimalloc
MIMALLOC_REPO = https://github.com/microsoft/mimalloc.git

# Set build directory and library name based on sanitizer mode
ifeq ($(SANITIZE),asan)
  MIMALLOC_BUILD_DIR = $(MIMALLOC_DIR)/out/asan
  MIMALLOC_STATIC_LIB = $(MIMALLOC_BUILD_DIR)/libmimalloc-asan.a
  CMAKE_EXTRA_FLAGS = -DMI_TRACK_ASAN=ON -DMI_OVERRIDE=OFF
else
  MIMALLOC_BUILD_DIR = $(MIMALLOC_DIR)/out/release
  MIMALLOC_STATIC_LIB = $(MIMALLOC_BUILD_DIR)/libmimalloc.a
  CMAKE_EXTRA_FLAGS = -DMI_OVERRIDE=ON
endif

MIMALLOC_INCLUDE = $(MIMALLOC_DIR)/include

# ck (Concurrency Kit)
CK_DIR = $(DEPS_ROOT)/ck
CK_REPO = https://github.com/concurrencykit/ck.git
CK_BUILD_DIR = $(CK_DIR)/out
CK_STATIC_LIB = $(CK_BUILD_DIR)/lib/libck.a
CK_INCLUDE = $(CK_BUILD_DIR)/include

# libtommath
LTM_DIR = $(DEPS_ROOT)/libtommath
LTM_REPO = https://github.com/libtom/libtommath.git
LTM_BUILD_DIR = $(LTM_DIR)/out/release
LTM_STATIC_LIB = $(LTM_BUILD_DIR)/libtommath.a
LTM_INCLUDE = $(LTM_DIR)

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

# Define source files
SRCS = gc.c base.c builtin.c bit_array.c

# Important: Only add include flag after mimalloc is built
ALL_CFLAGS = $(CFLAGS) $(SAN_FLAGS)
MIMALLOC_CFLAGS = -I$(MIMALLOC_INCLUDE)
CK_CFLAGS = -I$(CK_INCLUDE)
LTM_CFLAGS = -I$(LTM_INCLUDE)

TESTS = test_queue

default: compiled_result

# =============================================================
# Mimalloc build target - with shared caching
# =============================================================
.PHONY: mimalloc

mimalloc: $(MIMALLOC_STATIC_LIB)

$(MIMALLOC_STATIC_LIB):
	$(call log,Setting up mimalloc (SANITIZE=$(SANITIZE)))
	$(call log,Checking if mimalloc is already built...)
	$(if $(wildcard $(MIMALLOC_STATIC_LIB)),\
		$(call log,Mimalloc already built at $(MIMALLOC_STATIC_LIB) ✅),\
		$(call log,Mimalloc needs to be built);\
		mkdir -p $(DEPS_ROOT);\
		$(if $(wildcard $(MIMALLOC_DIR)),\
			$(call log,Mimalloc already cloned, proceeding with build),\
			$(call log,Cloning mimalloc to shared location...);\
			git clone $(MIMALLOC_REPO) $(MIMALLOC_DIR));\
		$(call log,Creating build directory $(MIMALLOC_BUILD_DIR));\
		mkdir -p $(MIMALLOC_BUILD_DIR);\
		$(call log,Configuring mimalloc with cmake...);\
		cd $(MIMALLOC_BUILD_DIR) && \
		cmake $(MIMALLOC_DIR) -DCMAKE_BUILD_TYPE=Release \
			$(CMAKE_EXTRA_FLAGS) \
			-DMI_BUILD_SHARED=OFF \
			-DMI_BUILD_TESTS=OFF;\
		$(call log,Building mimalloc...);\
		cd $(MIMALLOC_BUILD_DIR) && $(MAKE);\
		touch $(MIMALLOC_STATIC_LIB);\
		$(call log,Mimalloc built successfully at $(MIMALLOC_STATIC_LIB) ✅)\
	)

# =============================================================
# ck build target - with shared caching
# =============================================================
.PHONY: ck

ck: $(CK_STATIC_LIB)

$(CK_STATIC_LIB):
	$(call log,Setting up ck)
	$(call log,Checking if ck is already built...)
	$(if $(wildcard $(CK_STATIC_LIB)),\
		$(call log,ck already built at $(CK_STATIC_LIB) ✅),\
		$(call log,ck needs to be built);\
		mkdir -p $(DEPS_ROOT);\
		$(if $(wildcard $(CK_DIR)),\
			$(call log,ck already cloned, proceeding with build),\
			$(call log,Cloning ck to shared location...);\
			git clone $(CK_REPO) $(CK_DIR));\
		$(call log,Creating build directory $(CK_BUILD_DIR));\
		mkdir -p $(CK_BUILD_DIR);\
		$(call log,Configuring ck with autoconf...);\
		cd $(CK_DIR) && \
		./configure --prefix=$(CK_BUILD_DIR);\
		$(call log,Building ck...);\
		cd $(CK_DIR) && $(MAKE);\
		$(call log,Installing ck...);\
		$(MAKE) install;\
		touch $(CK_STATIC_LIB);\
		$(call log,ck built successfully at $(CK_STATIC_LIB) ✅)\
	)

# =============================================================
# LibTomMath build target - with shared caching
# =============================================================
.PHONY: libtommath

libtommath: $(LTM_STATIC_LIB)

$(LTM_STATIC_LIB):
	$(call log,Setting up libtommath)
	$(call log,Checking if libtommath is already built...)
	$(if $(wildcard $(TLTM_STATIC_LIB)),\
		$(call log,libtommath already built at $(LTM_STATIC_LIB) ✅),\
		$(call log,libtommath needs to be built);\
		mkdir -p $(DEPS_ROOT);\
		$(if $(wildcard $(LTM_DIR)),\
			$(call log,libtommath already cloned, proceeding with build),\
			$(call log,Cloning libtommath to shared location...);\
			git clone $(LTM_REPO) $(LTM_DIR));\
		$(call log,Creating build directory $(LTM_BUILD_DIR));\
		mkdir -p $(LTM_BUILD_DIR);\
		$(call log,Configuring libtommath with cmake...);\
		cd $(LTM_BUILD_DIR) && \
		cmake $(LTM_DIR) -DCMAKE_BUILD_TYPE=Release \
			$(CMAKE_EXTRA_FLAGS);\
		$(call log,Building libtommath...);\
		cd $(LTM_BUILD_DIR) && $(MAKE);\
		touch $(LTM_STATIC_LIB);\
		$(call log,libtommath built successfully at $(LTM_STATIC_LIB) ✅)\
	)

# =============================================================
# Object file rules
# Depend on mimalloc, ck and libtommath being built first
# =============================================================

%.o: %.c | mimalloc ck libtommath
	$(call log,Building $@)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) $(MIMALLOC_CFLAGS) $(CK_CFLAGS) $(LTM_CFLAGS) -c $< -o $@)

# =============================================================
# Main executable
# =============================================================

compiled_result: compiled_result.c gc.o base.o builtin.o bit_array.o
	$(call log,Building compiled_result)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) $(MIMALLOC_CFLAGS) $(CK_CFLAGS) $(LTM_CFLAGS )$^ $(MIMALLOC_STATIC_LIB) $(CK_STATIC_LIB) $(LTM_STATIC_LIB) -o $@)

# =============================================================
# Test targets
# =============================================================

test_queue: test_queue.c | mimalloc ck
	$(call log,Building test_queue)
	$(call log_cmd,$(CC) $(ALL_CFLAGS) $(MIMALLOC_CFLAGS) $(CK_CFLAGS) $(LTM_CFLAGS) $< $(MIMALLOC_STATIC_LIB) $(CK_STATIC_LIB) $(LTM_STATIC_LIB) -o $@)

# Note that test_base.c is not found, so removing it from the test targets
.PHONY: test
test: $(TESTS)
	$(call log,Running tests with ASAN_OPTIONS=verbosity=1)
	@for t in $(TESTS); do \
		$(call log,Running $$t); \
		ASAN_OPTIONS=verbosity=1 MallocNanoZone=0 ./$$t; \
	done

# Add a convenience target for ASAN builds
.PHONY: build-asan
build-asan:
	$(call log,Starting ASAN build)
	$(MAKE) clean
	$(MAKE) SANITIZE=asan

# =============================================================
# Utility targets
# =============================================================

.PHONY: show-flags
show-flags:
	$(call log,Compiler: $(CC))
	$(call log,CFLAGS: $(CFLAGS))
	$(call log,ALL_CFLAGS: $(ALL_CFLAGS))
	$(call log,MIMALLOC_CFLAGS: $(MIMALLOC_CFLAGS))
	$(call log,CK_CFLAGS: $(CK_CFLAGS))
	$(call log,LTM_CFLAGS: $(LTM_CFLAGS))
	$(call log,SANITIZE: $(SANITIZE))
	$(call log,SAN_FLAGS: $(SAN_FLAGS))
	$(call log,DEPS_ROOT: $(DEPS_ROOT))
	$(call log,MIMALLOC_DIR: $(MIMALLOC_DIR))
	$(call log,MIMALLOC_BUILD_DIR: $(MIMALLOC_BUILD_DIR))
	$(call log,MIMALLOC_STATIC_LIB: $(MIMALLOC_STATIC_LIB))

# =============================================================
# Clean targets
# =============================================================

.PHONY: clean clean-mimalloc clean-all
clean:
	$(call log,Cleaning local build artifacts)
	-rm -f *.o
	-rm -f test_queue
	-rm -f compiled_result

clean-mimalloc:
	$(call log,Cleaning mimalloc build directory)
	-rm -rf $(MIMALLOC_BUILD_DIR)

clean-ck:
	$(call log,Cleaning ck build directory)
	-rm -rf $(MIMALLOC_BUILD_DIR)


clean-libtommath:
	$(call log,Cleaning libtommath build directory)
	-rm -rf $(LTM_BUILD_DIR)

clean-all: clean
	$(call log,Cleaning all shared dependencies)
	-rm -rf $(DEPS_ROOT)
