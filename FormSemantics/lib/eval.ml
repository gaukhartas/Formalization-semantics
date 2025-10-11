open Lang

(* ------------------------------------------------------------ *)
(* Evaluation of expressions *)
(* ------------------------------------------------------------ *)

(* Lift operation + to IntV *)
let add_int_value v1 v2 = 
  match (v1, v2) with
  | (IntV i1, IntV i2) -> IntV (i1 + i2)
  | _ -> failwith "addition of non-int values"

(* Lift operation * to IntV *)
let mult_int_value v1 v2 = 
  match (v1, v2) with
  | (IntV i1, IntV i2) -> IntV (i1 * i2)
  | _ -> failwith "addition of non-int values"


let lift_arith_op = function
  | Add -> ( + )
  | Mult -> ( * )
  | Div -> ( / )

let apply_arith_op aop v1 v2 = 
  match (v1, v2) with
  | (IntV i1, IntV i2) -> IntV (lift_arith_op aop i1 i2)
  | _ -> failwith "arithmetic operation with non-int values"

let lift_bool_op = function
  | And -> ( && )
  | Or -> ( || )

let apply_bool_op bop v1 v2 = 
  match (v1, v2) with
  | (BoolV b1, BoolV b2) -> BoolV (lift_bool_op bop b1 b2)
  | _ -> failwith "boolean operation with non-bool values"

let lift_compar_op = function
  | Eq -> ( = )
  | LessEq -> ( <= )

let apply_compar_op bop v1 v2 = 
  match (v1, v2) with
  | (BoolV b1, BoolV b2) -> BoolV (lift_compar_op bop b1 b2)
  | (IntV i1, IntV i2) -> BoolV (lift_compar_op bop i1 i2)
  | _ -> failwith "comparison operation with incomparable values"

let apply_binop op v1 v2 =
  match op with
  | ArithOp aop -> apply_arith_op aop v1 v2
  | BoolOp bop -> apply_bool_op bop v1 v2
  | ComparOp cop -> apply_compar_op cop v1 v2
;;

(* Evaluation function for a value environment and an expression *)
let rec eval_expr env = function
  | Const c -> c
  | Var v -> List.assoc v env 
  | BinOp(op, e1, e2) -> apply_binop op (eval_expr env e1) (eval_expr env e2)
;;
