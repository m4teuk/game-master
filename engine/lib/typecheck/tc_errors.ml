type t = Load_error.t list ref

let create () : t = ref []

let report (errs : t) span msg =
  errs := Load_error.make Type span msg :: !errs

let collected (errs : t) = List.rev !errs

let is_empty (errs : t) =
  match !errs with
  | [] -> true
  | _ -> false
