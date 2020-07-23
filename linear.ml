
type boolean =
| True 
| False

type id = string

type ty = 
| B
| F of ty * ty

type tm =
| Var of id
| Bool of boolean
| If of tm * tm * tm
| Lam of tm * tm
| App of tm * tm
| Abs of id * ty * tm

module type Context = sig
type t
val empty : t
val lookup : t -> string -> ty
val extend : t -> string -> ty -> t
end

module Context : Context = struct
type t = (string * ty) list

let empty = []

let lookup ctx x =
try List.assoc x ctx
with Not_found -> failwith "Unbound variable"

let extend ctx x ty =
(x, ty) :: ctx
end



