#include "HsFFI.h"

struct distrib {
  StgInt64 count;
  StgDouble mean;
  StgDouble sum_sq_delta;
  StgDouble sum;
  StgDouble min;
  StgDouble max;
  volatile StgInt64 lock;
};

void hs_distrib_add_n(struct distrib* distrib, StgDouble val, StgInt64 n);

/*
 * Combine 'b' with 'a', writing the result in 'a'. Takes the lock of
 * 'b' while combining, but doesn't otherwise modify 'b'. 'a' is
 * assumed to not be used concurrently.
 */
void hs_distrib_combine(struct distrib* b, struct distrib* a);
