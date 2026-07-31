#include "config.h"
#include "ethread.h"

#include <assert.h>
#include <stdint.h>

__attribute__((import_module("experiment"), import_name("report")))
void report(int status, int initialized, int mutex_condition, int event,
            int atomics, int completed);
__attribute__((import_module("experiment"), import_name("progress")))
void progress(int phase);

enum { contention_threads = 2, child_threads = 3 };

typedef struct {
  int index;
  ethr_tid self;
} child_data;

static ethr_mutex mutex;
static ethr_cond ready_condition;
static ethr_cond release_condition;
static ethr_event wake_event;
static ethr_atomic32_t ready;
static ethr_atomic32_t counter;
static ethr_tsd_key tsd_key;
static int released;
static int tls_values[child_threads + 1];

static void verify_thread_state(child_data *data) {
  progress(100 + data->index);
  data->self = ethr_self();
  assert(ethr_tsd_set(tsd_key, &tls_values[data->index]) == 0);
  assert(ethr_tsd_get(tsd_key) == &tls_values[data->index]);
  progress(110 + data->index);
}

static void *contending_thread(void *argument) {
  child_data *data = argument;
  verify_thread_state(data);

  ethr_mutex_lock(&mutex);
  ethr_atomic32_inc(&ready);
  ethr_cond_signal(&ready_condition);
  while (!released) assert(ethr_cond_wait(&release_condition, &mutex) == 0);
  ethr_atomic32_inc(&counter);
  ethr_mutex_unlock(&mutex);

  return (void *)(intptr_t)(100 + data->index);
}

static void *event_thread(void *argument) {
  child_data *data = argument;
  verify_thread_state(data);

  ethr_mutex_lock(&mutex);
  ethr_atomic32_inc(&ready);
  ethr_cond_signal(&ready_condition);
  ethr_mutex_unlock(&mutex);
  assert(ethr_event_wait(&wake_event) == 0);
  ethr_atomic32_add(&counter, 10);
  return (void *)(intptr_t)(100 + data->index);
}

int main(void) {
  ethr_tid tids[child_threads];
  child_data data[child_threads];
  int main_tls = 42;

  progress(1);
  assert(ethr_init(NULL) == 0);
  progress(2);
  assert(ethr_late_init(NULL) == 0);
  progress(3);
  assert(ethr_mutex_init(&mutex) == 0);
  assert(ethr_cond_init(&ready_condition) == 0);
  assert(ethr_cond_init(&release_condition) == 0);
  assert(ethr_event_init(&wake_event) == 0);
  assert(ethr_tsd_key_create(&tsd_key, "wasi-probe") == 0);
  assert(ethr_tsd_set(tsd_key, &main_tls) == 0);
  ethr_atomic32_init(&ready, 0);
  ethr_atomic32_init(&counter, 0);
  progress(4);

  for (int index = 0; index < child_threads; index += 1) {
    data[index].index = index + 1;
    data[index].self = ethr_self();
    tls_values[index + 1] = 1000 + index;
  }

  for (int index = 0; index < contention_threads; index += 1) {
    progress(5 + index * 2);
    assert(ethr_thr_create(&tids[index], contending_thread, &data[index], NULL) == 0);
    progress(6 + index * 2);
  }
  progress(9);
  assert(ethr_thr_create(&tids[2], event_thread, &data[2], NULL) == 0);
  progress(10);

  ethr_mutex_lock(&mutex);
  while (ethr_atomic32_read(&ready) < child_threads) {
    assert(ethr_cond_wait(&ready_condition, &mutex) == 0);
  }
  released = 1;
  ethr_cond_broadcast(&release_condition);
  ethr_mutex_unlock(&mutex);
  ethr_event_set(&wake_event);
  progress(11);

  for (int index = 0; index < child_threads; index += 1) {
    void *result;
    assert(ethr_thr_join(tids[index], &result) == 0);
    assert((int)(intptr_t)result == 101 + index);
    assert(ethr_equal_tids(tids[index], data[index].self));
    assert(!ethr_equal_tids(ethr_self(), data[index].self));
  }
  progress(12);

  assert(ethr_tsd_get(tsd_key) == &main_tls);
  assert(ethr_atomic32_read(&counter) == 12);
  ethr_event_reset(&wake_event);
  assert(ethr_event_twait(&wake_event, 0) != 0);
  assert(ethr_tsd_key_delete(tsd_key) == 0);
  assert(ethr_event_destroy(&wake_event) == 0);
  assert(ethr_cond_destroy(&release_condition) == 0);
  assert(ethr_cond_destroy(&ready_condition) == 0);
  assert(ethr_mutex_destroy(&mutex) == 0);

  report(0, 1, 1, 1, 12, child_threads);
  return 0;
}
