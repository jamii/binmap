open Batteries_uni

module Array =
struct
  include Bigarray.Array1
  let geti array i = Bitmap.to_int (Bigarray.Array1.get array i)
  let seti array i v = Bigarray.Array1.set array i (Bitmap.of_int v)
end

(* ??? use layer/branch as implicit addressing, address in dynarray if layer mod _ = 0 ??? *)

type t =
    { length : int
    ; layers : int
    ; mutable array : (Bitmap.t, Bitmap.bigarray_elt, Bigarray.c_layout) Array.t
    ; pointers : Widemap.t
    ; mutable free : int } (* 0 is always the root of the tree so is also the end of the freelist *)

type node =
  | Bitmap of Bitmap.t
  | Pointer of int

let max_layers = Bitmap.width (* (ab)using bitmaps as node addresses *)
let max_length = max_int (* need ints to index bigarray *)

let layers_needed length =
  let bitmaps = (length / Bitmap.width) + 1 in
  let layers = int_of_float (ceil (log (float_of_int bitmaps) /. log 2.0)) in
  assert ((Int.pow 2 layers) * Bitmap.width >= length);
  assert ((Int.pow 2 (layers-1)) * Bitmap.width < length);
  layers

let create_array length =
  let array = Array.create Bitmap.bigarray_kind Bigarray.c_layout length in
  Array.fill array (Bitmap.of_int 0);
  array

let create length =
  let layers = layers_needed length in
  assert (length <= max_length);
  assert (layers <= max_layers);
  { length = length
  ; layers = layers
  ; array = create_array 2
  ; pointers = Widemap.create ()
  ; free = 0 }

let get_node binmap node_addr is_left =
  let index = node_addr + (if is_left then 0 else 1) in
  match Widemap.get binmap.pointers index with
  | false -> Bitmap (Array.get binmap.array index)
  | true -> Pointer (Array.geti binmap.array index)

let set_node binmap node_addr is_left node =
  let index = node_addr + (if is_left then 0 else 1) in
  match node with
  | Bitmap bitmap ->
      Widemap.set binmap.pointers index false;
      Array.set binmap.array index bitmap
  | Pointer int ->
      Widemap.set binmap.pointers index true;
      Array.seti binmap.array index int

(* double the size of a full array and then initialise the freelist *)
let grow_array binmap =
  assert (binmap.free = 0);
  let old_len = Array.dim binmap.array in
  assert (old_len <= max_int);
  let new_len = min max_int (2 * old_len) in
  let array = create_array new_len in
  Array.blit binmap.array (Array.sub array 0 old_len);
  binmap.array <- array;
  binmap.free <- old_len;
  for i = old_len to new_len-2 do
    Array.seti array i (i+1)
  done;
  Array.seti array (new_len - 1) 0

let add_pair binmap node_left node_right =
  (if binmap.free = 0 then grow_array binmap);
  let node_addr = binmap.free in
  let free_next = Array.geti binmap.array binmap.free in
  binmap.free <- free_next;
  set_node binmap node_addr true node_left;
  set_node binmap node_addr false node_right;
  node_addr

let del_pair binmap node_addr =
  Array.seti binmap.array node_addr (node_addr+1);
  Array.seti binmap.array (node_addr+1) binmap.free;
  binmap.free <- node_addr

let split_node binmap node_addr is_left bitmap =
  let child_left, child_right = Bitmap.split bitmap in
  let child_addr = add_pair binmap (Bitmap child_left) (Bitmap child_right) in
  set_node binmap node_addr is_left (Pointer child_addr);
  child_addr

let compact_node binmap node_addr is_left =
  match get_node binmap node_addr is_left with
  | Bitmap _ ->
      ignore ();
  | Pointer child_addr ->
      match (get_node binmap child_addr true, get_node binmap child_addr false) with
      | (Bitmap child_left, Bitmap child_right) when (Bitmap.is_paired child_left) && (Bitmap.is_paired child_right) ->
          del_pair binmap child_addr;
          set_node binmap node_addr is_left (Bitmap (Bitmap.join child_left child_right))
      | _ ->
          ignore ()

let rec get_loop binmap offset layer node_addr =
  (* this is a branch, decide which child we want *)
  let bin_size = 1 lsl layer in
  let middle = bin_size * Bitmap.width / 2 in
  let is_left = offset < middle in
  let node = get_node binmap node_addr is_left in
  let offset = if is_left then offset else offset-middle in
  (* handle child *)
  match node with
  | Bitmap bitmap ->
      assert (layer >= 0);
      Bitmap.get bitmap (offset / bin_size)
  | Pointer child_addr ->
      assert (layer >= 1);
      get_loop binmap offset (layer-1) child_addr

let get binmap i =
  assert (i < binmap.length);
  get_loop binmap i binmap.layers 0

let rec set_loop binmap offset set_layer set_bit layer node_addr =
  (* this is a branch, decide which child we want *)
  let bin_size = 1 lsl layer in
  let middle = bin_size * Bitmap.width / 2 in
  let is_left = (offset * 1 lsl set_layer) < middle in
  let node = get_node binmap node_addr is_left in
  let offset = if is_left then offset else offset-middle in
  (* handle child *)
  begin match node with
  | Bitmap bitmap when layer = set_layer->
      assert (layer >= 0);
      let bitmap = Bitmap.set bitmap (offset / bin_size) set_bit in
      set_node binmap node_addr is_left (Bitmap bitmap)
  | Bitmap bitmap when layer > set_layer->
      assert (layer >= 0);
      let child_addr = split_node binmap node_addr is_left bitmap in
      set_loop binmap offset set_layer set_bit (layer-1) child_addr
  | Pointer child_addr when layer = set_layer ->
      assert (layer >= 1);
      set_loop binmap (2*offset)     (set_layer-1) set_bit (layer-1) child_addr;
      set_loop binmap (2*offset + 1) (set_layer-1) set_bit (layer-1) child_addr
  | Pointer child_addr when layer > set_layer ->
      assert (layer >= 1);
      set_loop binmap offset set_layer set_bit (layer-1) child_addr end;
  compact_node binmap node_addr is_left

let set binmap i layer bit =
  assert (i < binmap.length);
  assert (layer >= 0);
  assert (layer <= binmap.layers);
  if get binmap i <> bit
  then set_loop binmap i layer bit binmap.layers 0
