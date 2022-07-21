open Core

type token_type =
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

type literal = STRING_LITERAL of string | NUMERIC_LITERAL of float | IDENTIFIER_LITERAL of string
  [@@deriving show, eq]

type t = {
  token_type : token_type;
  lexeme : string;
  literal: literal option;
  line : int;
  }
  [@@deriving show, eq]

let literal_option_show literal_option =
  match literal_option with
  | None -> "None"
  | Some literal -> "Some " ^ show_literal literal
  [@@deriving show, eq]

let my_show { token_type; lexeme; literal ; line} = sprintf "{ Token.token_type = %s; lexeme = \"%s\"; literal = %s; line = %d }" ( show_token_type token_type) lexeme (literal_option_show literal) line
