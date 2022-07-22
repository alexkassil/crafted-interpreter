open Core
open Token

type t = {
  source: string;
  start: int;
  current: int;
  line: int;
  tokens: Token.t list;
}

let keywords =
  Map.of_alist_exn (module String)
  [
    "and",    AND;
    "class",  CLASS;
    "else",   ELSE;
    "false",  FALSE;
    "for",    FOR;
    "fun",    FUN;
    "if",     IF;
    "nil",    NIL;
    "or",     OR;
    "print",  PRINT;
    "return", RETURN;
    "super",  SUPER;
    "this",   THIS;
    "true",   TRUE;
    "var",    VAR;
    "while",  WHILE;
  ]
let is_at_end scanner = (scanner.current >= String.length scanner.source)

let is_digit c = Char.('0' <= c && c <= '9')

let is_alpha c = Char.(('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

let add_token ({source; start; current; line; tokens} as scanner) token_type literal =
  printf "add_token: %s %d %d %d %s\n" source start current line (Token.show_token_type token_type);
  let text = String.sub source ~pos:start ~len:(current - start) in
  let token = {token_type = token_type; lexeme = text; literal = literal; line = line} in
  {scanner with tokens = token :: tokens}

let check_token scanner char =
  if is_at_end scanner then
    false, scanner
  (* else if not (phys_equal scanner.source.[scanner.current] char) then *)
  else if Char.(scanner.source.[scanner.current] <> char) then
    false, scanner
  else
    true, {scanner with current = scanner.current + 1}

let check_add_token scanner char true_case false_case literal =
  let matched, scanner = check_token scanner char in
  add_token scanner (if matched then true_case else false_case) literal

let advance scanner =
  (scanner.source.[scanner.current], {scanner with current = scanner.current + 1})

let peek scanner = if is_at_end scanner then '\x00' else scanner.source.[scanner.current]

let _peek_next scanner =
  if (scanner.current + 1 >= String.length scanner.source) then
    '\x00'
  else
    scanner.source.[scanner.current + 1]

let rec handle_comment scanner =
  if Char.(peek scanner = '\n') || is_at_end scanner then
    scanner
  else
    handle_comment @@ snd @@ advance scanner

let rec handle_string scanner error =
  if is_at_end scanner then
    (error scanner.line "Reached end of file while constructing string literal"; scanner)
  else
    let c, scanner = advance scanner in
    if Char.(c = '"') then
      let string_literal = String.sub scanner.source ~pos:(scanner.start + 1) ~len:(scanner.current - scanner.start - 2) in
      add_token scanner STRING (Some (STRING_LITERAL string_literal))
    else if Char.(c = '\n') then
      handle_string {scanner with line = scanner.line + 1} error
    else
      handle_string scanner error

let rec handle_number scanner is_float error =
  (* todo: add tests *)
  if not ((is_digit (peek scanner)) || ((not is_float) && Char.(peek scanner = '.'))) then
    let numeric_literal = String.sub scanner.source ~pos:scanner.start ~len:(scanner.current - scanner.start) in
    if not is_float then
      add_token scanner INTEGER (Some (INTEGER_LITERAL (int_of_string numeric_literal)))
    else
      add_token scanner FLOAT (Some (FLOAT_LITERAL (float_of_string numeric_literal)))
  else
    let c, scanner = advance scanner in
    if not is_float && Char.(c = '.') && is_digit (peek scanner) then
      handle_number scanner true error
    else if is_digit c then
      handle_number scanner is_float error
    else
      (error scanner.line "Error while constructing numeric literal"; scanner)

let rec handle_identifier scanner =
  let c = peek scanner in
  if not @@ is_alpha c || is_digit c || Char.(c = '_') then
    let identifier_literal = String.sub scanner.source ~pos:scanner.start ~len:(scanner.current - scanner.start) in
    match Map.find keywords identifier_literal with
    | Some keyword -> add_token scanner keyword (Some (IDENTIFIER_LITERAL identifier_literal))
    | None -> add_token scanner IDENTIFIER (Some (IDENTIFIER_LITERAL identifier_literal))
  else
    handle_identifier @@ snd @@ advance scanner


let scan_token scanner error =
  printf "scan_token: %s %d %d %d\n" scanner.source scanner.start scanner.current scanner.line;
  let c, scanner = advance scanner in
  let scanner = match c with
  | '(' -> add_token scanner LEFT_PAREN None
  | ')' -> add_token scanner RIGHT_PAREN None
  | '{' -> add_token scanner LEFT_BRACE None
  | '}' -> add_token scanner RIGHT_BRACE None
  | ',' -> add_token scanner COMMA None
  | '.' -> add_token scanner DOT None
  | '-' -> add_token scanner MINUS None
  | '+' -> add_token scanner PLUS None
  | ';' -> add_token scanner SEMICOLON None
  | '*' -> add_token scanner STAR None
  | '=' -> check_add_token scanner '=' EQUAL_EQUAL EQUAL None
  | '!' -> check_add_token scanner '=' BANG_EQUAL BANG None
  | '<' -> check_add_token scanner '=' LESS_EQUAL LESS None
  | '>' -> check_add_token scanner '=' GREATER_EQUAL GREATER None
  | '/' ->
    let matched, scanner = check_token scanner '/' in
    if matched then
      handle_comment scanner
    else
      add_token scanner SLASH None
  | ' '
  | '\r'
  | '\t'
    -> scanner
  | '\n' -> {scanner with line = scanner.line + 1}
  | '"' -> handle_string scanner error
  | c ->
    if is_digit c then
      handle_number scanner false error
    else if is_alpha c then
      handle_identifier scanner
    else
      (error scanner.line ("Unexpected character: " ^ String.of_char c); scanner)
  in
  scanner


let from_source source = {source = source; start = 0; current = 0; line = 0; tokens = []}

let scan_tokens source error =
  let scanner = from_source source in
  let rec go scanner =
    let scanner = {scanner with start = scanner.current} in
    if is_at_end scanner then
      List.rev ((add_token {scanner with start = scanner.current} EOF None).tokens)
    else
      let scanner = scan_token scanner error in
      go scanner
  in
  go scanner
