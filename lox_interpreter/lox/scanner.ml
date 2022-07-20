type t = {
  source: string;
  tokens: Token.t list;
}

let scanTokens {source; tokens} () =
  List.append tokens [{Token.tokenType = Token.LEFT_BRACE; lexeme = ""; literal = Some (Token.STRING_LITERAL source); line = 0;}]
