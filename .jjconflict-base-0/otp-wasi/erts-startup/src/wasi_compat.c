#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"
#include "sys.h"
#include "erl_alloc.h"
#include "erl_vm.h"

int BIN_VH_MIN_SIZE = VH_DEFAULT_SIZE;
erts_atomic32_t erts_writing_erl_crash_dump;
UWord sys_page_size = 65536;
UWord sys_large_page_size = 0;

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

void os_version(int *major, int *minor, int *build) {
  *major = 0;
  *minor = 0;
  *build = 0;
}

void os_flavor(char *buffer, unsigned int size) {
  const char flavor[] = "wasi";
  size_t length = sizeof(flavor) < size ? sizeof(flavor) : size;
  memcpy(buffer, flavor, length);
}

void erts_sys_alloc_init(void) {}

void *erts_sys_alloc(ErtsAlcType_t type, void *extra, Uint size) {
  (void)type;
  (void)extra;
  return malloc((size_t)size);
}

void *erts_sys_realloc(ErtsAlcType_t type, void *extra, void *pointer,
                       Uint size) {
  (void)type;
  (void)extra;
  return realloc(pointer, (size_t)size);
}

void erts_sys_free(ErtsAlcType_t type, void *extra, void *pointer) {
  (void)type;
  (void)extra;
  free(pointer);
}

void *erts_sys_aligned_alloc(UWord alignment, UWord size) {
  void *pointer = NULL;
  int error = posix_memalign(&pointer, (size_t)alignment, (size_t)size);
  if (error) errno = error;
  return error ? NULL : pointer;
}

void *erts_sys_aligned_realloc(UWord alignment, void *pointer, UWord size,
                               UWord old_size) {
  void *replacement = erts_sys_aligned_alloc(alignment, size);
  if (!replacement) return NULL;
  memcpy(replacement, pointer, (size_t)(old_size < size ? old_size : size));
  free(pointer);
  return replacement;
}

void erts_sys_aligned_free(UWord alignment, void *pointer) {
  (void)alignment;
  free(pointer);
}

void erts_exit(int code, const char *format, ...) {
  (void)format;
  exit(code);
}

void erts_thr_fatal_error(int error, const char *operation) {
  (void)operation;
  exit(error ? error : EXIT_FAILURE);
}

void erts_usage(void) {
  exit(EXIT_FAILURE);
}

void erl_assert_error(const char *expression, const char *function,
                      const char *file, int line) {
  (void)expression;
  (void)function;
  (void)file;
  (void)line;
  exit(EXIT_FAILURE);
}
