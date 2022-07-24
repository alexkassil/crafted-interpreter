(* open Token *)

type literal =
  | Int of int
  | Float of float
  | String of string
  | True
  | False
  | Nil
  [@@deriving show, eq]

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
  [@@deriving show, eq]


type unary_operator =
  | Negate
  | Not
  [@@deriving show, eq]


type expression =
  | Literal of literal
  | Grouping of grouping
  | Unary of unary
[@@deriving show, eq]
and grouping = expression
[@@deriving show, eq]
and unary = unary_operator * expression
[@@deriving show, eq]
and binary = expression * binary_operator * expression
[@@deriving show, eq]
