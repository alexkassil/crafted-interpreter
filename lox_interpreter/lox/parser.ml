(*
BNF grammar for parsing:

program        → statement* EOF;

statement      → exprStmt
               | printStmt ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;

expression     → equality ;
equality       → equality ( "!=" | "==" ) comparison
               | comparison ;
comparison     → comparison ( ">" | ">=" | "<" | "<=" ) term
               | term ;
term           → term ( "-" | "+" ) factor
               | factor ;
factor         → factor ( "/" | "*" ) unary
               | unary ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)

open Token
open Core

type program = Statements of statement list
and statement =
  | ExpressionStatement of exprStmt
  | PrintStatement of printStmt
and exprStmt = Expression of expression
and printStmt = Print of expression
and expression = Equality of equality
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
  [@@deriving show, eq]

type t = {
  tokens: Token.t array;
  current: int;
}

let peek parser = parser.tokens.(parser.current)

let is_at_end parser = Token.equal_token_type (peek parser).token_type EOF

let advance parser =
  if is_at_end parser then
    parser
  else
   { parser with current = parser.current + 1}

let check parser token =
  not (is_at_end parser) && Token.equal_token_type parser.tokens.(parser.current).token_type token

let matches parser token =
  (* printf "%s %d\n" (Token.show_token_type token) parser.current; *)
  if check parser token then
    true, advance parser
  else
    false, parser

let rec expression parser =
  Equality (fst @@ equality parser)
and equality parser =
  let rec equality_insert new_comparison builder = function
    | Comparison comparison -> builder new_comparison comparison
    | NotEqual (equality, comparison)  -> NotEqual (equality_insert new_comparison builder equality, comparison)
    | Equal (equality, comparison)  -> Equal (equality_insert new_comparison builder equality, comparison)
  in
  let left_expr, parser = comparison parser in
  let left_expr = Comparison left_expr in

  let matched, parser = (matches parser BANG_EQUAL) in
  if matched then
    let right_expr, parser = equality parser in
    let expr = equality_insert left_expr (fun x y -> NotEqual (x, y)) right_expr in
    expr, parser
  else

  let matched, parser = (matches parser EQUAL_EQUAL) in
  if matched then
    let right_expr, parser = equality parser in
    let expr = equality_insert left_expr (fun x y -> Equal (x, y)) right_expr in
    expr, parser
  else
    left_expr, parser
and comparison parser =
    let rec comparison_insert new_term builder = function
      | Term term -> builder new_term term
      | Greater (comparison, term) -> Greater (comparison_insert new_term builder comparison, term)
      | GreaterEqual (comparison, term) -> GreaterEqual (comparison_insert new_term builder comparison, term)
      | Less (comparison, term) -> Less (comparison_insert new_term builder comparison, term)
      | LessEqual (comparison, term) -> LessEqual (comparison_insert new_term builder comparison, term)
    in
    let left_expr, parser = term parser in
    let left_expr = Term left_expr in

    let matched, parser = (matches parser GREATER_EQUAL) in
    if matched then
      let right_expr, parser = comparison parser in
      let expr = comparison_insert left_expr (fun x y -> GreaterEqual (x, y)) right_expr in
      expr, parser
    else

    let matched, parser = (matches parser GREATER) in
    if matched then
      let right_expr, parser = comparison parser in
      let expr = comparison_insert left_expr (fun x y -> Greater (x, y)) right_expr in
      expr, parser
    else

    let matched, parser = (matches parser LESS_EQUAL) in
    if matched then
      let right_expr, parser = comparison parser in
      let expr = comparison_insert left_expr (fun x y -> LessEqual (x, y)) right_expr in
      expr, parser
    else

    let matched, parser = (matches parser LESS) in
    if matched then
      let right_expr, parser = comparison parser in
      let expr = comparison_insert left_expr (fun x y -> Less (x, y)) right_expr in
      expr, parser
    else
      left_expr, parser
and term parser =
  let rec term_insert new_factor builder = function
    | Factor factor -> builder new_factor factor
    | Plus (term, factor)  -> Plus (term_insert new_factor builder term, factor)
    | Minus (term, factor)  -> Minus (term_insert new_factor builder term, factor)
  in
  let left_expr, parser = factor parser in
  let left_expr = Factor left_expr in

  let matched, parser = (matches parser PLUS) in
  if matched then
    let right_expr, parser = term parser in
    let expr = term_insert left_expr (fun x y -> Plus (x, y)) right_expr in
    expr, parser
  else

  let matched, parser = (matches parser MINUS) in
  if matched then
    let right_expr, parser = term parser in
    let expr = term_insert left_expr (fun x y -> Minus (x, y)) right_expr in
    expr, parser
  else
    left_expr, parser
and factor parser =
    let rec factor_insert new_unary builder = function
      | Unary unary -> builder new_unary unary
      | Multiply (factor, unary)  -> Multiply (factor_insert new_unary builder factor, unary)
      | Divide (factor, unary)  -> Divide (factor_insert new_unary builder factor, unary)
    in
    let left_expr, parser = unary parser in
    let left_expr = Unary left_expr in

    let matched, parser = (matches parser STAR) in
    if matched then
      let right_expr, parser = factor parser in
      let expr = factor_insert left_expr (fun x y -> Multiply (x, y)) right_expr in
      expr, parser
    else

    let matched, parser = (matches parser SLASH) in
    if matched then
      let right_expr, parser = factor parser in
      let expr = factor_insert left_expr (fun x y -> Divide (x, y)) right_expr in
      expr, parser
    else
      left_expr, parser
and unary parser =
  let matched, parser = (matches parser BANG) in
  if matched then
    let expr, parser = unary parser in
    Not expr, parser
  else

  let matched, parser = (matches parser MINUS) in
  if matched then
    let expr, parser = unary parser in
    Negate expr, parser
  else
    let expr, parser = primary parser in
    Primary expr, parser

and primary parser =
  if fst @@ matches parser NIL then
    Nil, advance parser
  else if fst @@ matches parser TRUE then
    True, advance parser
  else if fst @@ matches parser FALSE then
      False, advance parser
  else if fst @@ matches parser STRING then
    match (peek parser).literal with
      | Some (STRING_LITERAL string) -> String string, advance parser
      | x -> failwith ("Invalid match on string token: " ^ literal_option_show x)
  else if fst @@ matches parser INTEGER then
    match (peek parser).literal with
      | Some (INTEGER_LITERAL int) -> Int int, advance parser
      | x -> failwith ("Invalid match on integer token: " ^ literal_option_show x)
  else if fst @@ matches parser FLOAT then
    match (peek parser).literal with
      | Some (FLOAT_LITERAL float) -> Float float, advance parser
      | x -> failwith ("Invalid match on float token: " ^ literal_option_show x)
  else
  let matched, parser = (matches parser LEFT_PAREN) in
  if matched then
    let expr = expression parser in
    (* TODO: consume til RIGHT_PAREN *)
    Group expr, advance parser
  else
    failwith ("Unknown token when parsing primary: " ^ Token.show (peek parser))
