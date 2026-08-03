#ifndef WASI_ERTS_CONFIG_H
#define WASI_ERTS_CONFIG_H

#include_next "config.h"

#undef HAVE_MMAP
#define HAVE_MMAP 0
#undef HAVE_MADVISE
#undef HAVE_POSIX_MADVISE

#endif
