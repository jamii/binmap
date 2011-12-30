(* !!! replace 0l by zero !!! *)

type t = Int32.t

type bigarray_elt = Batteries_uni.Bigarray.int32_elt
let bigarray_kind = Batteries_uni.Bigarray.int32

let width = 32

open Int32

let zeroes = 0l
let ones = lognot 0l

let get bitmap n =
  let bit = logand 1l (shift_right bitmap n) in
  bit > 0l

let set bitmap n = function
  | false -> logand bitmap (lognot (shift_left 1l n))
  | true -> logor bitmap (shift_left 1l n)

(* can be faster *)
let copy bitmap1 i bitmap2 j =
  set bitmap2 j (get bitmap1 i)

(* 010101010101.... *)
let paired_mask =
  let bitmap = ref 0l in
  for i = 0 to width/2 - 1 do
    bitmap := logor 1l (shift_left !bitmap 2)
  done;
  !bitmap

(* is bitmap made out of matching pairs of digits ie aabbccdd *)
let is_paired bitmap =
  let pairs = logxor bitmap (shift_right_logical bitmap 1) in
  (* pairs must have this pattern *0*0*0*0... *)
  logand paired_mask pairs = 0l

(* given two paired_up bitmaps, concat pairs and join bitmaps together *)
(* ie join aabb ccdd = abcd *)
let join bitmapL bitmapR =
  let bitmap = ref 0l in
  for i = 0 to width/2 - 1 do
    bitmap := copy bitmapR (2*i) !bitmap i
  done;
  for i = 0 to width/2 - 1 do
    bitmap := copy bitmapL (2*i) !bitmap (width/2 + i)
  done;
  !bitmap

(* given a bitmap, split into two paired_up bitmaps *)
(* split (join bitmap1 bitmap2) = (bitmap1, bitmap2) *)
let split bitmap =
  let bitmapL = ref 0l in
  let bitmapR = ref 0l in
  for i = 0 to width/2 - 1 do
    bitmapR := copy bitmap i !bitmapR (2*i);
    bitmapR := copy bitmap i !bitmapR (2*i + 1)
  done;
  for i = 0 to width/2 - 1 do
    bitmapL := copy bitmap (width/2 + i) !bitmapL (2*i);
    bitmapL := copy bitmap (width/2 + i) !bitmapL (2*i + 1)
  done;
  (!bitmapL, !bitmapR)

let to_string bitmap =
  let string = String.create width in
  for i = 0 to width-1 do
    let char = if (get bitmap i) then '1' else '0' in
    String.set string (width-i-1) char
  done;
  string

let of_string string =
  let bitmap = ref 0l in
  for i = 0 to width-1 do
    let char = String.get string (width-i-1) in
    bitmap := set !bitmap i (Pervasives.(=) char '1')
  done;
  !bitmap

let to_int = to_int
let of_int = of_int
