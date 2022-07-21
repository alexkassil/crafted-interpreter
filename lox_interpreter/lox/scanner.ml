open Core
open Token

type t = {
  source: string;
  start: int;
  current: int;
  line: int;
  tokens: Token.t list;
}

let is_at_end scanner = (scanner.current >= String.length scanner.source)

let add_token ({source; start; current; line; tokens} as scanner) token_type =
  printf "add_token: %s %d %d %d %s\n" source start current line (Token.show_tokenType token_type);
  let text = String.sub source ~pos:start ~len:(current - start) in
  let token = {tokenType = token_type; lexeme = text; literal = None; line = line} in
  {scanner with tokens = token :: tokens}

let check_token scanner char =
  if is_at_end scanner then
    false, scanner
  (* else if not (phys_equal scanner.source.[scanner.current] char) then *)
  else if Char.(scanner.source.[scanner.current] <> char) then
    false, scanner
  else
    true, {scanner with current = scanner.current + 1}

let check_add_token scanner char true_case false_case =
  let matched, scanner = check_token scanner char in
  add_token scanner (if matched then true_case else false_case)

let advance scanner =
  (scanner.source.[scanner.current], {scanner with current = scanner.current + 1})

let peek scanner = if is_at_end scanner then '\x00' else scanner.source.[scanner.current]

let rec handle_comment scanner =
  if Char.(peek scanner = '\n') || is_at_end scanner then
    scanner
  else
    handle_comment @@ snd @@ advance scanner

let scan_token scanner error =
  printf "scan_token: %s %d %d %d\n" scanner.source scanner.start scanner.current scanner.line;
  let c, scanner = advance scanner in
  let scanner = match c with
  | '(' -> add_token scanner LEFT_PAREN
  | ')' -> add_token scanner RIGHT_PAREN
  | '{' -> add_token scanner LEFT_BRACE
  | '}' -> add_token scanner RIGHT_BRACE
  | ',' -> add_token scanner COMMA
  | '.' -> add_token scanner DOT
  | '-' -> add_token scanner MINUS
  | '+' -> add_token scanner PLUS
  | ';' -> add_token scanner SEMICOLON
  | '*' -> add_token scanner STAR
  | '=' -> check_add_token scanner '=' EQUAL_EQUAL EQUAL
  | '!' -> check_add_token scanner '=' BANG_EQUAL BANG
  | '<' -> check_add_token scanner '=' LESS_EQUAL LESS
  | '>' -> check_add_token scanner '=' GREATER_EQUAL GREATER
  | '/' ->
    let matched, scanner = check_token scanner '/' in
    if matched then
      handle_comment scanner
    else
      add_token scanner SLASH
  | c -> error scanner.line ("Unexpected character: " ^ String.of_char c); scanner
  in
  scanner


let from_source source = {source = source; start = 0; current = 0; line = 0; tokens = []}

let scan_tokens source error =
  let scanner = from_source source in
  let rec go scanner =
    let scanner = {scanner with start = scanner.current} in
    if is_at_end scanner then
      List.rev ((add_token {scanner with start = scanner.current} EOF).tokens)
    else
      let scanner = scan_token scanner error in
      go scanner
  in
  go scanner
