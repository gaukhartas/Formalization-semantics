open Lang

(* ------------------------------------------------------------ *)
(* Typing of expressions *)
(* ------------------------------------------------------------ *)

let type_of_value = function
  | IntV _ -> IntT
  | BoolV _ -> BoolT


let type_binop op t1 t2 =
  match op with
  | ArithOp _aop ->
    if t1 = IntT && t2 = IntT
      then IntT
      else failwith "arithmetic operation not well-typed"
  | BoolOp _bop ->
    if t1 = BoolT && t2 = BoolT
      then BoolT
      else failwith "boolean operation not well-typed"
  | ComparOp _cop -> 
    if (t1 = t2)
      then BoolT
      else failwith "comparison operation not well-typed"
;;
  

(* Typing function for a type environment and an expression *)
let rec type_of_expr env = function 
  | Const c -> type_of_value c
  | Var v -> (match List.assoc_opt v env  with
              | None -> failwith ("Variable " ^ v ^ " not declared")
              | Some t -> t 
             )
  | BinOp(op, e1, e2) -> 
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      type_binop op t1 t2
;;
