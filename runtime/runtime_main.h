#ifndef RUNTIME_MAIN_H_
#define RUNTIME_MAIN_H_

long global_stack_size = 0;
long global_heap_size = 0;

static void c_entry_pt(void *data, object clo, int argc, object * args);
static void zyn_heap_init(long heap_size);

static void zyn_heap_init(long heap_size)
{
  /* Allocate heap area for second generation. */
#if DEBUG_SHOW_DIAG
  printf("main: Allocating and initializing heap...\n");
#endif
  gc_init_heap(heap_size);
  gc_start_collector();
}

#endif // RUNTIME_MAIN_H_
