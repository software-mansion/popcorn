#ifndef WASI_ERTS_COMPAT_H
#define WASI_ERTS_COMPAT_H

#include <pthread.h>
#include <signal.h>

void tzset(void);
void pthread_exit(void *result);
int pthread_sigmask(int how, const sigset_t *set, sigset_t *old_set);
int pthread_kill(pthread_t thread, int signal);
int sigwait(const sigset_t *set, int *signal);
int pipe(int descriptors[2]);

#endif
