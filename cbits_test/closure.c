#include <ffi.h>
#include <stdio.h>

static void foo(ffi_cif *cif, void *ret, void **args, void *user_data) {
  *((double *)ret) = (*((uint32_t *)(args[0]))) * (*((uint64_t *)(args[1])));
}

int main() {
  ffi_cif cif;
  ffi_type *atypes[] = {&ffi_type_sint32, &ffi_type_sint64};
  ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 2, &ffi_type_double, atypes);

  ffi_closure *closure;
  double (*code)(uint32_t, uint64_t);
  ffi_alloc_prep_closure(&closure, &cif, foo, NULL, &code);
  double rvalue = code(6, 7);
  ffi_closure_free(closure);
  printf("%f\n", rvalue);
}
