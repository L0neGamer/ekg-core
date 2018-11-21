#include "HsFFI.h"
#include "distrib.h"

static void hs_lock(volatile StgInt64* lock) {
  while(!__sync_bool_compare_and_swap(lock, 0, 1));
}

static void hs_unlock(volatile StgInt64* lock) {
  *lock = 0;
}

// Mean and variance are computed according to
// http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
void hs_distrib_add_n(struct distrib* distrib, StgDouble val, StgInt64 n) {
  hs_lock(&distrib->lock);
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
  hs_unlock(&distrib->lock);
}

// http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
void hs_distrib_combine(struct distrib* b, struct distrib* a) {
  hs_lock(&b->lock);
  const StgInt64 count = a->count + b->count;
  const StgDouble delta = b->mean - a->mean;
  const StgDouble mean = (a->count * a->mean + b->count * b->mean) / count;
  const StgDouble sum_sq_delta = (a->sum_sq_delta + b->sum_sq_delta +
                                  delta * delta * (a->count * b->count) / count);
  a->count = count;
  a->mean = mean;
  a->sum_sq_delta = sum_sq_delta;
  a->sum = a->sum + b->sum;
  a->min = b->min < a->min ? b->min : a->min;
  a->max = b->max > a->max ? b->max : a->max;
  hs_unlock(&b->lock);
}
