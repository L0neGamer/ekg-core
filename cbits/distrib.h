#include "HsFFI.h"

struct distrib {
  StgInt64 count;
  StgDouble mean;
  StgDouble sum_sq_delta;
  StgDouble sum;
  StgDouble min;
  StgDouble max;
};

void hs_distrib_add_n(struct distrib* distrib, StgDouble val, StgInt64 n);

void hs_distrib_combine(const struct distrib* b, struct distrib* a);
