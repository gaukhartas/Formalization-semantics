
(* ------------------------------------------------------------ *)
(* Definitions of data types *)
(* ------------------------------------------------------------ *)

(* Data type of values, either integer or boolean *)
type value = 
  |IntV of int
  |BoolV of bool
  [@@deriving show]


type arith_op = Add | Mult | Div
  [@@deriving show]

type bool_op = And | Or
  [@@deriving show]

type compar_op = Eq | LessEq
  [@@deriving show]

type operator =
  | ArithOp of arith_op
  | BoolOp of bool_op
  | ComparOp of compar_op
  [@@deriving show]

(* Data type of expresssions *)
type expr = 
  | Const of value
  | Var of string
  | BinOp of operator * expr * expr
  [@@deriving show]



(* Type for types *)

type tp = IntT | BoolT
  [@@deriving show]
