#ifndef WASI_ERTS_PWD_H
#define WASI_ERTS_PWD_H

#include <sys/types.h>

struct passwd {
  char *pw_name;
  char *pw_dir;
  uid_t pw_uid;
};

struct passwd *getpwuid(uid_t uid);

#endif
