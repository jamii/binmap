open Batteries_uni

type t = Bitmap.t DynArray.t

let create length = DynArray.init (length / Bitmap.width) (fun _ -> Bitmap.zeroes)

let get widemap i =
  try
    let bitmap = DynArray.get widemap (i / Bitmap.width) in
    Bitmap.get bitmap (i mod Bitmap.width)
  with
    DynArray.Invalid_arg _ -> false

let ensure_length widemap i =
  let last_index = i / Bitmap.width in
  for i = DynArray.length widemap to last_index do
    DynArray.add widemap Bitmap.zeroes
  done

let set widemap i bit =
  ensure_length widemap i;
  let bitmap = DynArray.get widemap (i / Bitmap.width) in
  let bitmap = Bitmap.set bitmap (i mod Bitmap.width) bit in
  DynArray.set widemap (i / Bitmap.width) bitmap

let to_string widemap =
  DynArray.fold_left
    (fun string bitmap ->
      string ^ (Bitmap.to_string bitmap) ^ ":")
    ":"
    widemap

let of_string string =
  let width = Bitmap.width + 1 in
  DynArray.init
    (String.length string / width)
    (fun i ->
      Bitmap.of_string (String.sub string (i*width + 1) width))
