open Parser

type lox_value =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Nil

let show_lox_value = function
  | Int int -> string_of_int int
  | Float float -> string_of_float float
  | String string -> "\"" ^ string ^ "\""
  | Bool bool -> string_of_bool bool
  | Nil -> "nil"

let is_truthy = function
  | Bool false -> false
  | Nil -> false
  | _ -> true


let rec eval_expression = function
  | Equality equality -> eval_equality equality
and eval_equality = function
  | Comparison comparison -> eval_comparison comparison
  | NotEqual (equality, comparison) ->
    (match (eval_equality equality, eval_comparison comparison) with
      | (Bool bool1, Bool bool2) -> Bool (bool1 != bool2)
      | _ -> failwith "Unknown")
  | Equal (equality, comparison) ->
    (match (eval_equality equality, eval_comparison comparison) with
      | (Bool bool1, Bool bool2) -> Bool (bool1 != bool2)
      | _ -> failwith "Unknown")
    and eval_comparison = function
  | Term term -> eval_term term
  | Greater (comparison, term) ->
    (match (eval_comparison comparison, eval_term term) with
    | (Int int1, Int int2) -> Bool (int1 > int2)
    | _ -> failwith "Unknown")
  | GreaterEqual (comparison, term) ->
    (match (eval_comparison comparison, eval_term term) with
    | (Int int1, Int int2) -> Bool (int1 >= int2)
    | _ -> failwith "Unknown")
  | Less (comparison, term) ->
    (match (eval_comparison comparison, eval_term term) with
    | (Int int1, Int int2) -> Bool (int1 < int2)
    | _ -> failwith "Unknown")
  | LessEqual (comparison, term) ->
    (match (eval_comparison comparison, eval_term term) with
    | (Int int1, Int int2) -> Bool (int1 <= int2)
    | _ -> failwith "Unknown")
and eval_term  = function
  | Factor factor -> eval_factor factor
  | Plus (term, factor)  ->
    (match (eval_term term, eval_factor factor) with
    | (Int int1, Int int2) -> Int (int1 + int2)
    | _ -> failwith "Unknown")
  | Minus (term, factor)  ->
    (match (eval_term term, eval_factor factor) with
    | (Int int1, Int int2) -> Int (int1 - int2)
    | _ -> failwith "Unknown")
  and eval_factor = function
  | Unary unary -> eval_unary unary
  | Multiply (factor, unary)  ->
    (match (eval_factor factor, eval_unary unary) with
      | (Int int1, Int int2) -> Int (int1 * int2)
      | _ -> failwith "Unknown")
  | Divide (factor, unary)  ->
    (match (eval_factor factor, eval_unary unary) with
      | (Int int1, Int int2) -> Int (int1 / int2)
      | _ -> failwith "Unknown")
and eval_unary = function
  | Primary primary -> eval_primary primary
  | Not unary -> Bool (not (is_truthy (eval_unary unary)))
  | Negate unary ->
    match eval_unary unary with
      | Int int -> Int (-int)
      | Float float -> Float (-.float)
      | _ -> failwith "Can't negate"
and eval_primary = function
  | Group expression -> eval_expression expression
  | Int int -> Int int
  | Float float -> Float float
  | String string -> String string
  | True -> Bool true
  | False -> Bool false
  | Nil -> Nil
