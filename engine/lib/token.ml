type t =
  | TYPE_IDENT of string
  | VALUE_IDENT of string
  | NUM_LIT of int
  | TEXT_LIT of string
  | KW_FN
  | KW_LET
  | KW_IN
  | KW_MATCH
  | KW_TYPE
  | KW_PILE
  | KW_OF
  | KW_VISIBILITY
  | KW_MOD
  | KW_IF
  | KW_THEN
  | KW_ELSE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | COLON
  | SEMI
  | EQ
  | EQEQ
  | NOTEQ
  | LTEQ
  | GTEQ
  | AMPAMP
  | PIPEPIPE
  | ARROW
  | PIPE
  | DOTDOT
  | LT
  | GT
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PIPE_FORWARD
  | EOF

type positioned = {
  token : t;
  span : Load_error.span;
}

let to_string = function
  | TYPE_IDENT s -> Printf.sprintf "TYPE_IDENT(%s)" s
  | VALUE_IDENT s -> Printf.sprintf "VALUE_IDENT(%s)" s
  | NUM_LIT n -> Printf.sprintf "NUM_LIT(%d)" n
  | TEXT_LIT s -> Printf.sprintf "TEXT_LIT(%S)" s
  | KW_FN -> "'fn'"
  | KW_LET -> "'let'"
  | KW_IN -> "'in'"
  | KW_MATCH -> "'match'"
  | KW_TYPE -> "'type'"
  | KW_PILE -> "'pile'"
  | KW_OF -> "'of'"
  | KW_VISIBILITY -> "'visibility'"
  | KW_MOD -> "'mod'"
  | KW_IF -> "'if'"
  | KW_THEN -> "'then'"
  | KW_ELSE -> "'else'"
  | LPAREN -> "'('"
  | RPAREN -> "')'"
  | LBRACE -> "'{'"
  | RBRACE -> "'}'"
  | LBRACK -> "'['"
  | RBRACK -> "']'"
  | COMMA -> "','"
  | COLON -> "':'"
  | SEMI -> "';'"
  | EQ -> "'='"
  | EQEQ -> "'=='"
  | NOTEQ -> "'!='"
  | LTEQ -> "'<='"
  | GTEQ -> "'>='"
  | AMPAMP -> "'&&'"
  | PIPEPIPE -> "'||'"
  | ARROW -> "'->'"
  | PIPE -> "'|'"
  | DOTDOT -> "'..'"
  | LT -> "'<'"
  | GT -> "'>'"
  | PLUS -> "'+'"
  | MINUS -> "'-'"
  | STAR -> "'*'"
  | SLASH -> "'/'"
  | PIPE_FORWARD -> "'|>'"
  | EOF -> "<eof>"
