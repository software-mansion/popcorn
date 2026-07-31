#include <assert.h>
#include <pthread.h>
#include <stdint.h>

__attribute__((import_module("experiment"), import_name("report")))
void report(int status, int phase, int counter, int completed);

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t condition = PTHREAD_COND_INITIALIZER;
static int ready;
static int released;
static int counter;
static _Thread_local int token;

static void *contending_thread(void *argument) {
  int index = (int)(intptr_t)argument;
  token = 100 + index;
  assert(token == 100 + index);

  assert(pthread_mutex_lock(&mutex) == 0);
  ready += 1;
  assert(pthread_cond_signal(&condition) == 0);
  while (!released) assert(pthread_cond_wait(&condition, &mutex) == 0);
  counter += 1;
  assert(pthread_mutex_unlock(&mutex) == 0);
  return (void *)(intptr_t)(1000 + index);
}

static void *stress_thread(void *argument) {
  int value = (int)(intptr_t)argument;
  assert(token == 0);
  token = value;
  assert(token == value);
  return argument;
}

int main(void) {
  pthread_t threads[3];

  for (int index = 0; index < 3; index += 1) {
    assert(pthread_create(&threads[index], NULL, contending_thread, (void *)(intptr_t)index) == 0);
  }

  assert(pthread_mutex_lock(&mutex) == 0);
  while (ready != 3) assert(pthread_cond_wait(&condition, &mutex) == 0);
  released = 1;
  assert(pthread_cond_broadcast(&condition) == 0);
  assert(pthread_mutex_unlock(&mutex) == 0);

  for (int index = 0; index < 3; index += 1) {
    void *result;
    assert(pthread_join(threads[index], &result) == 0);
    assert((int)(intptr_t)result == 1000 + index);
  }
  assert(counter == 3);

  for (int index = 1; index <= 100; index += 1) {
    pthread_t thread;
    void *result;
    assert(pthread_create(&thread, NULL, stress_thread, (void *)(intptr_t)index) == 0);
    assert(pthread_join(thread, &result) == 0);
    assert((int)(intptr_t)result == index);
  }

  report(0, 2, counter, 103);
  return 0;
}
