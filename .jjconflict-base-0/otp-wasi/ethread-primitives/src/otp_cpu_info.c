#include "erl_misc_utils.h"

struct erts_cpu_info_t_ {
  int configured;
};

static struct erts_cpu_info_t_ cpu_info = {8};

erts_cpu_info_t *erts_cpu_info_create(void) {
  return &cpu_info;
}

int erts_get_cpu_configured(erts_cpu_info_t *info) {
  return info->configured;
}
