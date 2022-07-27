(*
BNF grammar for parsing:

expression     → equality ;
equality       → equality ( "!=" | "==" ) comparison
               | comparison
comparison     → comparison ( ">" | ">=" | "<" | "<=" ) term
               | term;
term           → term ( "-" | "+" ) factor
               | factor;
factor         → factor ( "/" | "*" ) unary
               | unary;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)

open Token
open Ast

type expression = Equality of equality
and equality =
  | Comparison of comparison
  | NotEqual of equality * comparison
  | Equal of equality * comparison
and comparison =
  | Term of term
  | Greater of comparison * term
  | GreaterEqual of comparison * term
  | Less of comparison * term
  | LessEqual of comparison * term
and term =
  | Factor of factor
  | Plus of term * factor
  | Minus of term * factor
and factor =
  | Unary of unary
  | Divide of factor * unary
  | Multiply of factor * unary
and unary =
  | Primary of primary
  | Not of unary
  | Negate of unary
and primary =
  | Group of expression
  | Int of int
  | Float of float
  | String of string
  | True
  | False
  | Nil



type t = {
  tokens: Token.t array;
  current: int;
}

let peek parser = parser.tokens.(parser.current)

let is_at_end parser = (peek parser).token_type == EOF

let advance parser =
  if is_at_end parser then
    parser
  else
   { parser with current = parser.current + 1}

let check parser token =
  not (is_at_end parser) && parser.tokens.(parser.current).token_type == token

let matches parser token =
  if check parser token then
    true, advance parser
  else
    false, parser

let rec expresssion parser =
  equality parser

(* t1 == t2 == t3 == t4 *)
(* Equal (Equal (Equal (t1, t2), t3), t4) *)
(* t2 = Equal (t1, t2) *)
(* t2 == t3 == t4 *)
(* Equal (Equal (t2, t3), t4) *)

and equality parser =
  let left_expr, parser = comparison parser in

  let matched, parser = (matches parser BANG_EQUAL) in
  if matched then
    let right_expr, parser = equality parser in
    right_expr, parser
  else
    left_expr, parser

and comparison parser = Nil, parser
