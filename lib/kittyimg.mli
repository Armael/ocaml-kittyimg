(** An OCaml implementation for the client-side terminal graphics protocol of
    the Kitty terminal emulator. See {{:
    https://sw.kovidgoyal.net/kitty/graphics-protocol/} the protocol
    specification} for more information. *)

val string_of_bytes_ba :
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  string

type pixel_format =
  [ `RGB (* 24 bits RGB *)
  | `RGBA (* 32 bits RGBA *)
  | `PNG ]

module Id : sig
  type t = private int
  val of_int : int -> t
end

module Placement : sig
  type t = private int
  val of_int : int -> t
end

type display_opts

val display_opts :
  ?placement:Placement.t ->
  ?x:int -> ?y:int ->
  ?w:int -> ?h:int ->
  ?xoff:int -> ?yoff:int ->
  ?cstretch:int -> ?rstretch:int ->
  ?move_cursor:bool ->
  ?zindex:int ->
  ?quiet:[`Default | `OK | `Failure] ->
  unit ->
  display_opts

type send_mode =
  [ `Display of display_opts
  | `Store of Id.t ]

val send_image :
  w:int -> h:int ->
  format:pixel_format ->
  ?quiet:[`Default | `OK | `Failure] ->
  ?mode:send_mode ->
  string ->
  unit

val display_image : ?opts:display_opts -> Id.t -> unit

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

val delete_image : free:bool -> delete_action -> unit
