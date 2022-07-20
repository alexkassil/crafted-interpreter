open Core

type tokenType =
  (* Single character tokens *)
  LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |
  COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR |
  (* One or two character tokens *)
  BANG | BANG_EQUAL |
  EQUAL | EQUAL_EQUAL |
  GREATER | GREATER_EQUAL |
  LESS | LESS_EQUAL |
  (* Literals *)
  IDENTIFIER | STRING | NUMBER |
  (* Keywords *)
  AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR |
  PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE |
  (* Special EOF marker *)
  EOF
  [@@deriving show, eq]


type 'literal t = {
  tokenType : tokenType;
  lexeme : string;
  literal: 'literal;
  line : int;
  literal_to_string : 'literal -> string;
  }


  let show { tokenType; lexeme; literal ; line; literal_to_string} = sprintf "{ Token.tokenType = %s; lexeme = \"%s\"; literal = \"%s\"; line = %d }" ( show_tokenType tokenType) lexeme (literal_to_string literal) line
