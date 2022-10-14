(*
BNF grammar for parsing:

program        → declaration* EOF;

declaration    → varDecl
               | statement ;

varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

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
               | "(" expression ")" | IDENTIFIER ;
*)

open Token
open Core

type program = Statements of statement list
and statement =
  | VariableDeclarartion of Token.t * expression option
  | ExpressionStatement of expression
  | PrintStatement of expression
(* and exprStatement = Expression of expression
and printStatement = Print of expression *)
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
  | Identifier of string
  [@@deriving show, eq]

let show_statements statements = String.concat ~sep:"\n" (List.map ~f:show_statement statements)

type t = {
  tokens: Token.t array;
  current: int;
  error: Token.t -> string -> unit;
}

exception ParseError

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

let consume parser token message =
  if check parser token then
    peek parser, advance parser
  else
    (parser.error (peek parser) message;
    raise ParseError;)

let rec synchronize parser =
  let token_type = (peek parser).token_type in
  let parser = advance parser in
  if Token.equal_token_type token_type Token.SEMICOLON then
    parser
  else
    let token_type = (peek parser).token_type in
    if List.exists ~f:(Token.equal_token_type token_type) [Token.CLASS;Token.FUN;Token.VAR;Token.FOR;Token.IF;Token.WHILE;Token.PRINT;Token.RETURN] then
      parser
    else
      synchronize parser

let rec declaration parser =
  (* TODO: try/catch block w/ ParseError and synchronize *)
  let matched, parser = matches parser VAR in
  if matched then
    var_declaration parser
  else
    statement parser
and var_declaration parser =
  let name, parser = consume parser IDENTIFIER "Expect variable name" in
  let matched, parser = matches parser EQUAL in
  if matched then
    let expr, parser = expression parser in
    let _, parser = consume parser SEMICOLON "Expect ';' after variable declaration" in
    VariableDeclarartion (name, Some expr), parser
  else
    let _, parser = consume parser SEMICOLON "Expect ';' after variable declaration" in
    VariableDeclarartion (name, None), parser
and statement parser =
  let matched, parser = (matches parser PRINT) in
  if matched then
    print_statement parser
  else
    expression_statement parser
and print_statement parser =
    let expr, parser = expression parser in
    let _, parser = consume parser SEMICOLON "Expect ';' after value." in
    PrintStatement expr, parser
and expression_statement parser =
    let expr, parser = expression parser in
    let _, parser = consume parser SEMICOLON "Expect ';' after expression." in
    ExpressionStatement expr, parser
and expression parser =
  let expr, parser = equality parser in
  Equality expr, parser
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
      | Some (IDENTIFIER_LITERAL string) -> Identifier string, advance parser
      | x -> failwith ("Invalid match on identifier token: " ^ literal_option_show x)
  else if fst @@ matches parser INTEGER then
    match (peek parser).literal with
      | Some (INTEGER_LITERAL int) -> Int int, advance parser
      | x -> failwith ("Invalid match on integer token: " ^ literal_option_show x)
  else if fst @@ matches parser FLOAT then
    match (peek parser).literal with
      | Some (FLOAT_LITERAL float) -> Float float, advance parser
      | x -> failwith ("Invalid match on float token: " ^ literal_option_show x)
  else if fst @@ matches parser IDENTIFIER then
    match (peek parser).literal with
      | Some (STRING_LITERAL string) -> String string, advance parser
      | x -> failwith ("Invalid match on string token: " ^ literal_option_show x)
  else
  let matched, parser = (matches parser LEFT_PAREN) in
  if matched then
    let expr, parser = expression parser in
    let _, parser = consume parser RIGHT_PAREN "Expect ')' after expression" in
    Group expr, parser
  else
    failwith ("Unknown token when parsing primary: " ^ Token.show (peek parser))

let parse parser =
  let rec helper parser list =
    if is_at_end parser then
      list
    else
      let stmt, parser = declaration parser in
      helper parser (stmt :: list)
  in
    List.rev @@ helper parser []

let parenthesize expressions = "(" ^ String.concat ~sep:" " expressions ^ ")"

let rec show_statement_pp = function
  | VariableDeclarartion (name, expression) -> parenthesize ["="; name.lexeme; Option.value (Option.(>>|) expression show_expression_pp) ~default:""]
  | PrintStatement expression -> parenthesize ["print"; show_expression_pp expression]
  | ExpressionStatement expression -> show_expression_pp expression
and show_expression_pp = function
  | Equality equality -> show_equality_pp equality
and show_equality_pp = function
  | Comparison comparison -> show_comparison_pp comparison
  | Equal (equality, comparison) -> parenthesize ["=="; show_equality_pp equality; show_comparison_pp comparison]
  | NotEqual (equality, comparison) -> parenthesize ["!="; show_equality_pp equality; show_comparison_pp comparison]
    and show_comparison_pp = function
  | Term term -> show_term_pp term
  | Greater (comparison, term) -> parenthesize [">"; show_comparison_pp comparison; show_term_pp term]
  | GreaterEqual (comparison, term) -> parenthesize [">="; show_comparison_pp comparison; show_term_pp term]
  | Less (comparison, term) -> parenthesize ["<"; show_comparison_pp comparison; show_term_pp term]
  | LessEqual (comparison, term) -> parenthesize ["<="; show_comparison_pp comparison; show_term_pp term]
and show_term_pp  = function
  | Factor factor -> show_factor_pp factor
  | Plus (term, factor) -> parenthesize ["+"; show_term_pp term; show_factor_pp factor]
  | Minus (term, factor) -> parenthesize ["-"; show_term_pp term; show_factor_pp factor]
  and show_factor_pp = function
  | Unary unary -> show_unary_pp unary
  | Multiply (factor, unary) -> parenthesize ["*"; show_factor_pp factor; show_unary_pp unary]
  | Divide (factor, unary) -> parenthesize ["/"; show_factor_pp factor; show_unary_pp unary]
and show_unary_pp = function
  | Primary primary -> show_primary_pp primary
  | Not unary -> parenthesize ["!"; show_unary_pp unary]
  | Negate unary -> parenthesize ["-"; show_unary_pp unary]
and show_primary_pp = function
  | Group expression -> show_expression_pp expression
  | Int int -> string_of_int int
  | Float float -> string_of_float float
  | String string -> string
  | True -> string_of_bool true
  | False -> string_of_bool false
  | Nil -> "nil"
  | Identifier string -> string

let show_statements_pp statements = String.concat ~sep:"\n" (List.map ~f:show_statement_pp statements)
