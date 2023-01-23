let string_of_bytes_ba ba =
  let nbytes = Bigarray.Array1.size_in_bytes ba in
  let buf = Bytes.create nbytes in
  for i = 0 to nbytes - 1 do
    Bytes.set_uint8 buf i ba.{i}
  done;
  Bytes.unsafe_to_string buf

type pixel_format =
  [ `RGB (* 24 bits RGB *)
  | `RGBA (* 32 bits RGBA *)
  | `PNG ]

let string_of_pixel_format = function
  | `RGB -> "24"
  | `RGBA -> "32"
  | `PNG -> "100"

let esc = Char.chr 0o33

module Id : sig
  type t = private int
  val of_int : int -> t
end = struct
  type t = int
  let of_int i =
    (* XX *)
    assert (i > 0 && i <= 4294967295);
    i
end

module Placement : sig
  type t = private int
  val of_int : int -> t
end = struct
  type t = int
  let of_int i =
    (* XX *)
    assert (i > 0 && i <= 4294967295);
    i
end

type display_opts = {
  placement : Placement.t option;
  (* source rectangle *)
  x : int option;
  y : int option;
  w : int option;
  h : int option;
  (* target *)
  xoff : int option;
  yoff : int option;
  (* stretch mode *)
  cstretch : int option;
  rstretch : int option;
  (* cursor movement policy *)
  move_cursor : bool option;
  (* zindex *)
  zindex : int option;
  (* quiet *)
  quiet : [ `Default | `OK | `Failure ] option;
}

let display_opts
    ?placement
    ?x ?y ?w ?h
    ?xoff ?yoff
    ?cstretch ?rstretch
    ?move_cursor
    ?zindex
    ?quiet
    ()
  =
  { placement; x; y; w; h; xoff; yoff;
    cstretch; rstretch; move_cursor; zindex; quiet }

let items_of_display_opts opts =
  [
    'p', (opts.placement :> int option);
    'x', opts.x;
    'y', opts.y;
    'w', opts.w;
    'h', opts.h;
    'X', opts.xoff;
    'Y', opts.yoff;
    'c', opts.cstretch;
    'r', opts.rstretch;
    'C', Option.map (function true -> 0 | false -> 1) opts.move_cursor;
    'z', opts.zindex;
    'q',
    Option.map (function `Default -> 0 | `OK -> 1 | `Failure -> 2)
      opts.quiet;
  ]
  |> List.filter_map (fun (k, o) ->
    Option.map (fun v -> (k, string_of_int v)) o
  )

type send_mode =
  [ `Display of display_opts
  | `Store of Id.t ]

let items_of_send_mode = function
  | `Display dopts -> ('a', "T") :: items_of_display_opts dopts
  | `Store id -> [('i', string_of_int (id : Id.t :> int))]

let print_code control_items data =
  let items_s =
    List.map (fun (k, v) -> Printf.sprintf "%c=%s" k v) control_items
    |> String.concat ","
  in
  Printf.printf "%c_G%s;%s%c\\%!" esc items_s data esc

let send_image ~w ~h ~format ?(quiet = `Default) ?(mode = `Display (display_opts ())) data =
  let data64 = Base64.encode_exn data in
  let rec loop i =
    let j = min (i + 4096) (String.length data64) in
    let chunk = String.sub data64 i (j-i) in
    let is_first = (i = 0) in
    let is_last = (j = String.length data64) in
    if is_first then
      print_code (('f', string_of_pixel_format format) ::
                  ('s', string_of_int w) ::
                  ('v', string_of_int h) ::
                  ('m', if is_last then "0" else "1") ::
                  ('q', match quiet with `Default -> "0" | `OK -> "1" | `Failure -> "2") ::
                  items_of_send_mode mode)
        chunk
    else
      print_code [('m', if is_last then "0" else "1")]
        chunk;
    if not is_last then loop j
  in
  loop 0

let display_image ?(opts = display_opts ()) (id: Id.t) =
  print_code (('a', "p") ::
              ('i', string_of_int (id :> int)) ::
              items_of_display_opts opts)
    ""

type delete_action =
  [ `All
  | `Id of Id.t * Placement.t option
  (* TODO: newest *)
  | `Cursor
  | `Frames
  | `Cell of int * int
  | `CellZ of int * int * int
  | `Column of int
  | `Row of int
  | `Zindex of int ]

let items_of_delete_action (free: bool) (a: delete_action) =
  match a with
  | `All ->
    [('d', if free then "A" else "a")]
  | `Id (id, placement) ->
    ('d', if free then "I" else "i") ::
    ('i', string_of_int (id :> int)) ::
    (match placement with
     | None -> []
     | Some pid -> [('p', string_of_int (pid :> int))])
  | `Cursor ->
    [('d', if free then "C" else "c")]
  | `Frames ->
    [('d', if free then "F" else "f")]
  | `Cell (x, y) ->
    [('d', if free then "P" else "p");
     ('x', string_of_int x);
     ('y', string_of_int y)]
  | `CellZ (x, y, z) ->
    [('d', if free then "Q" else "q");
     ('x', string_of_int x);
     ('y', string_of_int y);
     ('z', string_of_int z)]
  | `Column x ->
    [('d', if free then "X" else "x");
     ('x', string_of_int x)]
  | `Row y ->
    [('d', if free then "Y" else "y");
     ('y', string_of_int y)]
  | `Zindex z ->
    [('d', if free then "Z" else "z");
     ('z', string_of_int z)]

let delete_image ~free a =
  print_code (('a', "d") :: items_of_delete_action free a) ""
