open Core
open Token

  type t = {
    source: string;
    start: int;
    current: int;
    line: int;
    tokens: Token.t list;
  }

  let add_token ({source; start; current; line; tokens} as scanner) token_type =
    printf "add_token: %s %d %d %d %s\n" source start current line (Token.show_tokenType token_type);
    let text = String.sub source ~pos:start ~len:(current - start) in
    let token = {tokenType = token_type; lexeme = text; literal = None; line = line} in
    {scanner with tokens = token :: tokens}


  let advance scanner =
    (scanner.source.[scanner.current], {scanner with current = scanner.current + 1})

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
    | c -> error scanner.line ("Unexpected character: " ^ String.of_char c); scanner
    in
    scanner


  let from_source source = {source = source; start = 0; current = 0; line = 0; tokens = []}

  let scan_tokens source error =
    let scanner = from_source source in
    let rec go scanner =
      let scanner = {scanner with start = scanner.current} in
      if (scanner.current >= String.length scanner.source) then
        List.rev ((add_token {scanner with start = scanner.current} EOF).tokens)
      else
        let scanner = scan_token scanner error in
        go scanner
    in
    go scanner

