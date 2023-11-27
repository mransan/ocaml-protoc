
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

inline int pbrt_varint_size(int64_t i) {
  int n = 0;
  while (1) {
    n++;
    int64_t cur = i & 0x7f;
    if (cur == i)
      break;
    i = i >> 7;
  }
  return n;
}

// number of bytes for i
CAMLprim value caml_pbrt_varint_size(int64_t i) {
  int res = pbrt_varint_size(i);
  return Val_int(res);
}

CAMLprim value caml_pbrt_varint_size_byte(value v_i) {
  CAMLparam1(v_i);

  int64_t i = Int64_val(v_i);
  int res = pbrt_varint_size(i);
  CAMLreturn(Val_int(res));
}

// write i at str[idx…]
inline void pbrt_varint(unsigned char *str, int64_t i) {
  while (true) {
    int64_t cur = i & 0x7f;
    if (cur == i) {
      *str = (unsigned char)cur;
      break;
    } else {
      *str = (unsigned char)(cur | 0x80);
      i = i >> 7;
      ++str;
    }
  }
}

//    let[@inline] varint (i : int64) (e : t) : unit =
//      let n_bytes = varint_size i in
//      let start = reserve_n e n_bytes in
//
//      let i = ref i in
//      for j = 0 to n_bytes - 1 do
//        let cur = Int64.(logand !i 0x7fL) in
//        if j = n_bytes - 1 then
//          Bytes.set e.b (start + j) (Char.unsafe_chr Int64.(to_int cur))
//        else (
//          Bytes.set e.b (start + j)
//            (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
//          i := Int64.shift_right_logical !i 7
//        )
//      done

// write `i` starting at `idx`
CAMLprim value caml_pbrt_varint(value _str, intnat idx, int64_t i) {
  CAMLparam1(_str);
  char *str = Bytes_val(_str);
  pbrt_varint(str + idx, i);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_pbrt_varint_bytes(value _str, value _idx, value _i) {
  CAMLparam3(_str, _idx, _i);
  char *str = Bytes_val(_str);
  int idx = Int_val(_idx);
  int64_t i = Int64_val(_idx);
  pbrt_varint(str + idx, i);
  CAMLreturn(Val_unit);
}
