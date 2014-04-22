#include "HsFFI.h"

void hs_lock(volatile StgInt64* lock) {
  while(!__sync_bool_compare_and_swap(lock, 0, 1));
}

void hs_unlock(volatile StgInt64* lock) {
  *lock = 0;
}
