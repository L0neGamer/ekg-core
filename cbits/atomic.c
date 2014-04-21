#include "HsFFI.h"

void hs_atomic_add(volatile StgInt64* atomic, StgInt64 n) {
  __sync_fetch_and_add(atomic, n);
}

void hs_atomic_subtract(volatile StgInt64* atomic, StgInt64 n) {
  __sync_fetch_and_sub(atomic, n);
}

StgInt64 hs_atomic_read(volatile const StgInt64* atomic) {
  return *atomic;
}

void hs_atomic_write(volatile StgInt64* atomic, StgInt64 n) {
  *atomic = n;
}
