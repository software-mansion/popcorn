#include "config.h"
#include "sys.h"
#include "erl_process.h"
#include "erl_alloc.h"
#include "erl_bif_unique.h"
#include "erl_monitor_link.h"
#include "erl_port_task.h"
#include "erl_check_io.h"
#include "erl_proc_sig_queue.h"
#include "erl_time.h"
#include "erl_thr_progress.h"
#include "erl_thr_queue.h"
#include "erl_threads.h"

static void *thread_alloc(ErtsAlcType_t type, size_t size) {
  return erts_alloc_fnf(type, (Uint)size);
}

static void *thread_realloc(ErtsAlcType_t type, void *pointer, size_t size) {
  return erts_realloc_fnf(type, pointer, (Uint)size);
}

static void thread_free(ErtsAlcType_t type, void *pointer) {
  erts_free(type, pointer);
}

static void *standard_alloc(size_t size) {
  return thread_alloc(ERTS_ALC_T_ETHR_STD, size);
}

static void *standard_realloc(void *pointer, size_t size) {
  return thread_realloc(ERTS_ALC_T_ETHR_STD, pointer, size);
}

static void standard_free(void *pointer) {
  thread_free(ERTS_ALC_T_ETHR_STD, pointer);
}

static void *short_lived_alloc(size_t size) {
  return thread_alloc(ERTS_ALC_T_ETHR_SL, size);
}

static void *short_lived_realloc(void *pointer, size_t size) {
  return thread_realloc(ERTS_ALC_T_ETHR_SL, pointer, size);
}

static void short_lived_free(void *pointer) {
  thread_free(ERTS_ALC_T_ETHR_SL, pointer);
}

static void *long_lived_alloc(size_t size) {
  return thread_alloc(ERTS_ALC_T_ETHR_LL, size);
}

static void *long_lived_realloc(void *pointer, size_t size) {
  return thread_realloc(ERTS_ALC_T_ETHR_LL, pointer, size);
}

static void long_lived_free(void *pointer) {
  thread_free(ERTS_ALC_T_ETHR_LL, pointer);
}

int main(void) {
  char *arguments[] = {"erts-wasi-init", NULL};
  int argument_count = 1;
  ErtsAllocInitOpts allocator_options = ERTS_ALLOC_INIT_DEF_OPTS_INITER;
  erts_thr_init_data_t thread_options = ERTS_THR_INIT_DATA_DEF_INITER;
  erts_thr_late_init_data_t late_options = ERTS_THR_LATE_INIT_DATA_DEF_INITER;

  erts_thr_init(&thread_options);
  erts_init_sys_time_sup();
  erts_thr_progress_pre_init();
  erts_atomic32_init_nob(&erts_writing_erl_crash_dump, 0);

  erts_no_schedulers = 1;
  erts_no_dirty_cpu_schedulers = 1;
  erts_no_dirty_io_schedulers = 1;
  erts_early_init_scheduling(3);

  allocator_options.ncpu = 1;
  erts_alloc_init(&argument_count, arguments, &allocator_options);
  erts_init_check_io(&argument_count, arguments);
  erts_thr_progress_init(1, 4, 2);
  erts_thr_q_init();

  late_options.mem.std.alloc = standard_alloc;
  late_options.mem.std.realloc = standard_realloc;
  late_options.mem.std.free = standard_free;
  late_options.mem.sl.alloc = short_lived_alloc;
  late_options.mem.sl.realloc = short_lived_realloc;
  late_options.mem.sl.free = short_lived_free;
  late_options.mem.ll.alloc = long_lived_alloc;
  late_options.mem.ll.realloc = long_lived_realloc;
  late_options.mem.ll.free = long_lived_free;
  late_options.main_threads = 4;
  late_options.reader_groups = 1;
  erts_thr_late_init(&late_options);

  erts_monitor_link_init();
  erts_bif_unique_init();
  erts_proc_sig_queue_init();
  erts_init_time(0, ERTS_NO_TIME_WARP_MODE);
  erts_init_process(1, 1024, 0);
  erts_init_scheduling(1, 1, 1, 1, 1, 1);
  return 0;
}
