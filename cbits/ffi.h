#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

typedef enum { FFI_DEFAULT_ABI } ffi_abi;

typedef struct {
  size_t size;
  unsigned short alignment;
  uint8_t type;
} ffi_type;

extern const ffi_type ffi_type_void;
extern const ffi_type ffi_type_uint8;
extern const ffi_type ffi_type_sint8;
extern const ffi_type ffi_type_uint16;
extern const ffi_type ffi_type_sint16;
extern const ffi_type ffi_type_uint32;
extern const ffi_type ffi_type_sint32;
extern const ffi_type ffi_type_uint64;
extern const ffi_type ffi_type_sint64;
extern const ffi_type ffi_type_float;
extern const ffi_type ffi_type_double;
extern const ffi_type ffi_type_pointer;

typedef struct {
  ffi_abi abi;
  unsigned nargs;
  ffi_type **arg_types;
  ffi_type *rtype;
  unsigned encoding;
} ffi_cif;

typedef enum {
  FFI_OK = 0,
  FFI_BAD_TYPEDEF,
  FFI_BAD_ABI,
  FFI_BAD_ARGTYPE
} ffi_status;

ffi_status ffi_prep_cif(ffi_cif *cif, ffi_abi abi, unsigned int nargs,
                        ffi_type *rtype, ffi_type **atypes);

void ffi_call(ffi_cif *cif, void (*fn)(void), void *rvalue, void **avalue);

typedef char ffi_closure;

ffi_status ffi_alloc_prep_closure(ffi_closure **pclosure, ffi_cif *cif,
                                  void (*fun)(ffi_cif *cif, void *ret,
                                              void **args, void *user_data),
                                  void *user_data, void **code);

void ffi_closure_free(void *);

#ifdef __cplusplus
}
#endif
