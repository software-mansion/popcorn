#include "config.h"
#include "ethread.h"

#include <assert.h>
#include <errno.h>
#include <stdint.h>

__attribute__((import_module("experiment"), import_name("report")))
void report(int status, int required_children, int completed, int distinct_ids,
            int distinct_tsd, int synchronized, int capacity_error);
__attribute__((import_module("experiment"), import_name("progress")))
void progress(int role, int phase);

enum {
  normal_scheduler,
  dirty_cpu_scheduler,
  dirty_io_scheduler,
  auxiliary,
  poll,
  system_message_dispatcher,
  capacity_spare,
  overflow_probe,
  child_count
};

enum { required_children = 6, live_children = 7 };

typedef struct {
  int role;
  ethr_tid identity;
  int tsd_value;
} child_data;

static ethr_mutex mutex;
static ethr_cond ready_condition;
static ethr_cond release_condition;
static ethr_tsd_key tsd_key;
static ethr_atomic32_t ready;
static ethr_atomic32_t completed;
static int released;

static void *long_lived_thread(void *argument) {
  child_data *data = argument;

  data->identity = ethr_self();
  assert(ethr_tsd_set(tsd_key, &data->tsd_value) == 0);
  assert(ethr_tsd_get(tsd_key) == &data->tsd_value);
  progress(data->role, 1);

  ethr_mutex_lock(&mutex);
  ethr_atomic32_inc(&ready);
  ethr_cond_signal(&ready_condition);
  while (!released) assert(ethr_cond_wait(&release_condition, &mutex) == 0);
  assert(ethr_tsd_get(tsd_key) == &data->tsd_value);
  ethr_mutex_unlock(&mutex);

  ethr_atomic32_inc(&completed);
  progress(data->role, 2);
  return (void *)(intptr_t)(100 + data->role);
}

int main(void) {
  ethr_tid tids[live_children];
  child_data data[live_children];
  ethr_tid overflow_tid;
  child_data overflow = {.role = overflow_probe, .tsd_value = 1008};
  int main_tsd = 42;
  int capacity_error;

  progress(-1, 1);
  assert(ethr_init(NULL) == 0);
  assert(ethr_late_init(NULL) == 0);
  assert(ethr_mutex_init(&mutex) == 0);
  assert(ethr_cond_init(&ready_condition) == 0);
  assert(ethr_cond_init(&release_condition) == 0);
  assert(ethr_tsd_key_create(&tsd_key, "erts-topology") == 0);
  assert(ethr_tsd_set(tsd_key, &main_tsd) == 0);
  ethr_atomic32_init(&ready, 0);
  ethr_atomic32_init(&completed, 0);

  for (int role = 0; role < live_children; role += 1) {
    data[role].role = role;
    data[role].identity = ethr_self();
    data[role].tsd_value = 1000 + role;
    assert(ethr_thr_create(&tids[role], long_lived_thread, &data[role], NULL) == 0);
  }

  ethr_mutex_lock(&mutex);
  while (ethr_atomic32_read(&ready) != live_children) {
    assert(ethr_cond_wait(&ready_condition, &mutex) == 0);
  }
  progress(-1, 2);

  capacity_error = ethr_thr_create(&overflow_tid, long_lived_thread, &overflow, NULL);
  assert(capacity_error == EAGAIN);

  released = 1;
  ethr_cond_broadcast(&release_condition);
  ethr_mutex_unlock(&mutex);

  for (int role = 0; role < live_children; role += 1) {
    void *result;
    assert(ethr_thr_join(tids[role], &result) == 0);
    assert((int)(intptr_t)result == 100 + role);
    assert(ethr_equal_tids(tids[role], data[role].identity));
    assert(!ethr_equal_tids(ethr_self(), data[role].identity));
    for (int other = 0; other < role; other += 1) {
      assert(!ethr_equal_tids(data[role].identity, data[other].identity));
      assert(&data[role].tsd_value != &data[other].tsd_value);
    }
  }

  assert(ethr_tsd_get(tsd_key) == &main_tsd);
  assert(ethr_atomic32_read(&completed) == live_children);
  assert(ethr_tsd_key_delete(tsd_key) == 0);
  assert(ethr_cond_destroy(&release_condition) == 0);
  assert(ethr_cond_destroy(&ready_condition) == 0);
  assert(ethr_mutex_destroy(&mutex) == 0);

  report(0, required_children, live_children, 1, 1, 1, capacity_error);
  return 0;
}
