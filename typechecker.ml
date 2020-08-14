
type bop =
| Add
| Mult
| Leq

type id = string

type expr = 
| Var of id
| Int of int
| Bool of bool
| Binop of (bop * expr * expr)
| If of (expr * expr * expr)
| Let of (id * expr * expr)

type typ = 
| TInt
| TBool

module type Context = sig
  type t
  val empty : t
  val lookup : t -> id -> typ
  val extend : t -> id -> typ -> t
end

module Context : Context  = struct
  type t = (id * typ) list
  
  let empty = []

  let lookup ctx x = 
    try List.assoc x ctx
    with Not_found -> failwith "No variable found in context!"

  let extend ctx x ty = 
    (x, ty) :: ctx
end

open Context

let rec typeof ctx e : typ = 
match e with
| Var x -> lookup ctx x
| Int _ -> TInt
| Bool _ -> TBool
| Binop (bop, e1, e2) -> typeof_bop ctx bop e1 e2
| If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
| Let (x, e1, e2) -> typeof_let ctx x e1 e2

and typeof_bop ctx bop e1 e2 = 
  let t1 = typeof ctx e1 in
  let t2 = typeof ctx e2 in
  match bop, t1, t2 with
  | Add, TInt, TInt | Mult, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> failwith "type check failed!"

and typeof_if ctx e1 e2 e3 = 
  match (typeof ctx e1) with
  | TBool -> let t2 = typeof ctx e2 in
             (match typeof ctx e3 with
             | n when n = t2 -> t2
             | _ -> failwith "type check failed!")
  | _ -> failwith "type check failed!"

and typeof_let ctx x e1 e2 = 
  let t1 = typeof ctx e1 in
  let ctx' = extend ctx x t1 in
  typeof ctx' e2

(* The typechecker people have access to *)
let typecheck e = typeof empty e
;;

let print_type t =
  match t with
  | TInt -> "Its type is integer."
  | TBool -> "Its type is bool."
;;

let ctx = 
  let ctx1 = extend empty "x" TInt in
  let ctx2 = extend ctx1 "y" TInt in
    extend ctx2 "b" TBool
;;

let e1 = 
  let g = Var "b" in
  let e1 = Binop (Add, Var "x", Var "y") in
  let e2 = Var "x" in 
  If(g, e1, e2)
;;

let test = 
  print_endline (print_type (typeof ctx e1))
;;


