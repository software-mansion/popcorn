#ifndef WASI_ERTS_CONFIG_H
#define WASI_ERTS_CONFIG_H

#include_next "config.h"

#undef HAVE_MMAP
#define HAVE_MMAP 0
#undef HAVE_MADVISE
#undef HAVE_POSIX_MADVISE
#undef HAVE_MLOCKALL
#undef HAVE_DECL_DAYLIGHT
#define HAVE_DECL_DAYLIGHT 0
#undef HAVE_DLOPEN

#endif
