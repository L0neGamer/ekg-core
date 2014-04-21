#include "HsFFI.h"
#include "distrib.h"

// Mean and variance are computed according to
// http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
void hs_distrib_add_n(struct distrib* distrib, StgDouble val, StgInt64 n) {
  const StgInt64 count = distrib->count + n;
  const StgDouble delta = val - distrib->mean;
  const StgDouble mean = distrib->mean + n * delta / count;
  const StgDouble sum_sq_delta = distrib->sum_sq_delta + delta * (val - mean) * n;
  distrib->count = count;
  distrib->mean = mean;
  distrib->sum_sq_delta = sum_sq_delta;
  distrib->sum += val;
  distrib->min = val < distrib->min ? val : distrib->min;
  distrib->max = val > distrib->max ? val : distrib->max;
}
