#include <ffi.h>
#include <stdio.h>

static double foo(uint32_t x, uint64_t y) { return x * y; }

int main() {
  ffi_cif cif;
  ffi_type *atypes[] = {&ffi_type_sint32, &ffi_type_sint64};
  ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 2, &ffi_type_double, atypes);
  double rvalue;
  uint32_t x = 6;
  uint64_t y = 7;
  void *avalue[] = {&x, &y};
  ffi_call(&cif, foo, &rvalue, avalue);
  printf("%f\n", rvalue);
}
