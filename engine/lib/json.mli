(** Minimal JSON representation and parser used by [Render] for the
    canonical tagged-union wire format (stdlib §14 JSON variants).

    v0 keeps numbers to integers — the [.game] type system has no
    floats, and allowing [J_num of float] would invite rounding games.
    Re-visit if the language grows a [Float] type later. *)

type t =
  | J_null
  | J_bool of bool
  | J_num of int
  | J_text of string
  | J_array of t list
  | J_object of (string * t) list

val parse : string -> (t, string) result
(** Recursive-descent parser. Accepts canonical JSON with integer
    numbers, text strings (with standard backslash escapes for
    quote/backslash/slash and [\n], [\t], [\r]), booleans, null,
    arrays, and objects. Rejects floats, NaN/Infinity, trailing
    commas, and unquoted keys. On failure, returns a message with
    the byte offset. *)

val to_string : ?pretty:bool -> t -> string
(** Renders a [t] to JSON text. [~pretty:true] emits a 2-space
    indented form; the default is a compact single-line form
    suitable for wire protocols and the action log. *)
