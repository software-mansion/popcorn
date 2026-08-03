#include "config.h"
#include "erl_process.h"

int main(void) {
  erts_init_scheduling(1, 1, 1, 1, 1, 1);
  return 0;
}
