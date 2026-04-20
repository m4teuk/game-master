type span = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

let no_span = {
  file = "<none>";
  start_line = 0;
  start_col = 0;
  end_line = 0;
  end_col = 0;
}

type category =
  | Lex
  | Parse
  | Type
  | Link

type t = {
  category : category;
  span : span;
  message : string;
}

let make category span message = { category; span; message }

let category_to_string = function
  | Lex -> "lex"
  | Parse -> "parse"
  | Type -> "type"
  | Link -> "link"

let to_string { category; span; message } =
  Printf.sprintf "%s:%d:%d: %s: %s"
    span.file span.start_line span.start_col
    (category_to_string category) message
