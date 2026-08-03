#include <errno.h>
#include <pthread.h>

int pthread_attr_setscope(pthread_attr_t *attributes, int scope) {
  (void)attributes;
  (void)scope;
  return ENOTSUP;
}

int pipe(int descriptors[2]) {
  (void)descriptors;
  errno = ENOSYS;
  return -1;
}

void tzset(void) {}
