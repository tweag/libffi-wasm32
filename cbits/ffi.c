#include <ffi.h>

static const ffi_type ffi_type_i32 = {.size = 4, .alignment = 4, .type = 0};
static const ffi_type ffi_type_i64 = {.size = 8, .alignment = 8, .type = 1};
static const ffi_type ffi_type_f32 = {.size = 4, .alignment = 4, .type = 2};
static const ffi_type ffi_type_f64 = {.size = 8, .alignment = 8, .type = 3};

const ffi_type ffi_type_void = {.size = 0, .alignment = 0, .type = 0};

extern const ffi_type ffi_type_uint8 __attribute__((alias("ffi_type_i32")));
extern const ffi_type ffi_type_sint8 __attribute__((alias("ffi_type_i32")));
extern const ffi_type ffi_type_uint16 __attribute__((alias("ffi_type_i32")));
extern const ffi_type ffi_type_sint16 __attribute__((alias("ffi_type_i32")));
extern const ffi_type ffi_type_uint32 __attribute__((alias("ffi_type_i32")));
extern const ffi_type ffi_type_sint32 __attribute__((alias("ffi_type_i32")));
extern const ffi_type ffi_type_uint64 __attribute__((alias("ffi_type_i64")));
extern const ffi_type ffi_type_sint64 __attribute__((alias("ffi_type_i64")));
extern const ffi_type ffi_type_float __attribute__((alias("ffi_type_f32")));
extern const ffi_type ffi_type_double __attribute__((alias("ffi_type_f64")));
extern const ffi_type ffi_type_pointer __attribute__((alias("ffi_type_i32")));

ffi_status ffi_prep_cif(ffi_cif *cif, ffi_abi abi, unsigned int nargs,
                        ffi_type *rtype, ffi_type **atypes) {
  cif->abi = abi;
  cif->nargs = nargs;
  cif->arg_types = atypes;
  cif->rtype = rtype;

  unsigned acc = 1;
  if (rtype != &ffi_type_void) {
    acc = (acc << 2) | rtype->type;
  }
  for (unsigned i = 0; i < nargs; ++i) {
    acc = (acc << 2) | atypes[i]->type;
  }
  acc = (acc << 1) | (rtype != &ffi_type_void);
  cif->encoding = acc;

  return FFI_OK;
}
