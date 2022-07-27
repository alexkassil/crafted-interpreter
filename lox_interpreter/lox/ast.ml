type literal =
  | Int of int
  | Float of float
  | String of string
  | True
  | False
  | Nil

type binary_operator =
  | Equal
  | NotEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Plus
  | Minus
  | Multiply
  | Divide

type unary_operator =
  | Negate
  | Not

type expression =
  | Literal of literal
  | Grouping of grouping
  | Unary of unary
  | Binary of binary
and grouping = Group of expression
and unary = unary_operator * expression
and binary = expression * binary_operator * expression

let test = Binary (
  Unary (Negate, Literal (Int 123)),
  Multiply,
  Grouping (Group (Literal (Float 45.67)))
)

let parenthesize expressions = "(" ^ String.concat " " expressions ^ ")"

let show_literal = function
| Int int -> string_of_int int
| Float float -> string_of_float float
| String string -> string
| True -> "true"
| False -> "false"
| Nil -> "nil"

let rec show_expression = function
  | Literal literal -> show_literal literal
  | Grouping grouping -> show_grouping grouping
  | Unary unary -> show_unary unary
  | Binary binary -> show_binary binary
and show_grouping (Group expression) = parenthesize ["group"; show_expression expression]
and show_unary = function
  | (Not, expression) -> parenthesize ["!"; show_expression expression]
  | (Negate, expression) -> parenthesize ["-"; show_expression expression]
and show_binary = function
  | (left, Equal, right) -> parenthesize ["=="; show_expression left; show_expression right]
  | (left, NotEqual, right) -> parenthesize ["!="; show_expression left; show_expression right]
  | (left, Greater, right) -> parenthesize [">"; show_expression left; show_expression right]
  | (left, GreaterEqual, right) -> parenthesize [">="; show_expression left; show_expression right]
  | (left, Less, right) -> parenthesize ["<"; show_expression left; show_expression right]
  | (left, LessEqual, right) -> parenthesize ["<="; show_expression left; show_expression right]
  | (left, Plus, right) -> parenthesize ["+"; show_expression left; show_expression right]
  | (left, Minus, right) -> parenthesize ["-"; show_expression left; show_expression right]
  | (left, Multiply, right) -> parenthesize ["*"; show_expression left; show_expression right]
  | (left, Divide, right) -> parenthesize ["/"; show_expression left; show_expression right]
