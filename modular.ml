module type VARIABLE = sig
  type t
  val fresh : string -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val to_user_string : t -> string
end

module Var : VARIABLE = struct
  type t = string * int
  let counter = ref 0
  let fresh n = (n, (counter := !counter + 1; !counter))
  let equal (_, id1) (_, id2) = id1 = id2
  let compare (_, id1) (_, id2) = id2 - id1
  let to_string (s, n) = s ^ "@" ^ string_of_int n
  let to_user_string (s, _) = s
end

module type OPERATOR = sig
  type t
  val to_string : t -> string
  val arity : t -> int list
end


(* ABT implementation with thanks to Joseph Abrahamson *)

module type ABT = sig
  module Variable : VARIABLE
  module Operator : OPERATOR

  type t

  type 'a view =
    | V of Variable.t
    | L of Variable.t * 'a
    | A of Operator.t * 'a list

  exception Malformed

  val into : t view -> t
  val out  : t -> t view

  val aequiv : t -> t -> bool
  val map : ('a -> 'b) -> ('a view -> 'b view)
end

module type ABT_UTIL = sig
  include ABT

  val freevars : t -> Variable.t list
  val subst : t -> Variable.t -> t -> t

  val var : Variable.t -> t
  val lam : Variable.t * t -> t
  val ($) : Operator.t -> t list -> t

  val to_string : t -> string
end

module Abt (V : VARIABLE) (O : OPERATOR) : ABT
  with type Variable.t = V.t
   and type Operator.t = O.t =
struct
  module Variable = V
  module Operator = O

  type 'a view =
    | V of Variable.t
    | L of Variable.t * 'a
    | A of Operator.t * 'a list

  type t =
    | Free of Variable.t
    | Bound of int
    | Lambda of t
    | Operator of Operator.t * t list

  exception Malformed

  let rec map_variables
      (free  : int -> Variable.t -> t)
      (bound : int -> int -> t)
      (tm : t)
    : t
    = let rec aux l tm = match tm with
      | Free v -> free l v
      | Bound n -> bound l n
      | Lambda t -> Lambda (aux (l+1) t)
      | Operator (o, ts) -> Operator (o, List.map (aux l) ts)
    in aux 0 tm

  let bind (v : Variable.t) (tm : t) : t =
    let free n v' = if Variable.equal v v' then Bound n else Free v' in
    let bound n m = Bound m in
    Lambda (map_variables free bound tm)

  (** Invariant, this is a term just underneath a lambda binder *)
  let unbind (tm : t) : Variable.t * t =
    let v = Variable.fresh "x" in
    let free n v' = if Variable.equal v v' then raise Malformed else Free v' in
    let bound _ m = if m = 0 then Free v else Bound m in
    (v, map_variables free bound tm)

  let into = function
    | V v -> Free v
    | L (v, tm) -> bind v tm
    | A (op, args) ->
      if (List.length (Operator.arity op) = List.length args)
      then Operator (op, args)
      else raise Malformed

  let out = function
    | Free v -> V v
    | Bound _ -> raise Malformed
    | Lambda tm -> let (var, tm') = unbind tm in L (var, tm')
    | Operator (op, args) -> A (op, args)

  let rec aequiv x y = match x, y with
    | Free vx            , Free vy            -> V.equal vx vy
    | Bound nx           , Bound ny           -> nx = ny
    | Lambda tx          , Lambda ty          -> aequiv tx ty
    | Operator (ox, axs) , Operator (oy, ays) ->
      ox = oy && List.for_all2 aequiv axs ays
    | _                  , _                  -> false

  let map f = function
    | V v -> V v
    | L (v, a)  -> L (v, f a)
    | A (o, al) -> A (o, List.map f al)

end

module Abt_Util (A : ABT) = struct
  include A

  let var v        = into (V v)
  let lam (v, e)   = into (L (v, e))
  let ($$) op args = into (A (op, args))

  let rec subst tm var exp =
    match out exp with
    | V v -> if Variable.equal var v then tm else exp
    | L (v, exp') -> lam (v, subst tm var exp')
    | A (op, args) -> op $$ List.map (subst tm var) args

  module VarSet : Set.S with type elt := Variable.t
    = Set.Make(Variable)

  let freevars exp =
    let rec aux exp =
      match out exp with
      | V v          -> VarSet.singleton v
      | L (v, exp')  -> VarSet.remove v (aux exp')
      | A (op, exps) -> List.fold_left VarSet.union VarSet.empty (List.map aux exps)
    in VarSet.elements (aux exp)

end

type 'a judgement =
  | Chk of 'a * 'a

module type KERNEL = sig
  module Op : sig
    type t = ..
  end
end

module type UNIT = sig
  module Op : OPERATOR
  exception Call

  module Typing : functor (Syntax : ABT with type Operator.t = Op.t) -> sig
    val judge : Syntax.t judgement -> unit
  end
end

module One (M : KERNEL) = struct
  exception Call

  type M.Op.t +=
    | UNIT
    | AX

  module Op = struct
    type t = M.Op.t
    let to_string o =
      match o with
      | UNIT -> "unit"
      | AX -> "ax"
      | _ -> raise Call

    let arity o =
      match o with
      | UNIT -> []
      | AX -> []
      | _ -> raise Call
  end

  module Typing (Syntax : ABT with type Operator.t = Op.t) = struct
    open Syntax
    
    let judge j =
      match j with
      | Chk (m, a) ->
        match (out m, out a) with
        | (A (AX, _), A (UNIT, _)) -> ()
        | _ -> raise Call
  end
end

module Pi (M : KERNEL) = struct
  exception Call

  type M.Op.t +=
    | LAM
    | PI

  module Op = struct
    type t = M.Op.t
   
    let to_string o =
      match o with
      | LAM -> "lam"
      | PI -> "pi"
      | _ -> raise Call

    let arity o =
      match o with
      | PI -> [0;1]
      | LAM -> [1]
      | _ -> raise Call
  end

  module Typing (Syntax : ABT with type Operator.t = Op.t) = struct
    open Syntax
    
    let judge j =
      match j with
      | Chk (m, a) ->
        match (out m, out a) with
        | (A (LAM, e), A (PI, zz)) -> () (* TODO: make proper *)
        | _ -> raise Call
  end
end

module Sg (M : KERNEL) = struct
  exception Call

  module Op = struct
    type t = M.Op.t
    type M.Op.t += PAIR
   
    let to_string o =
      match o with
      | PAIR -> "pair"
      | _ -> raise Call

    let arity o =
      match o with
      | PAIR -> [0;0]
      | _ -> raise Call
  end

  module Typing (Syntax : ABT with type Operator.t = Op.t) = struct
    let judge j =
      match j with
      | Chk (m, a) -> raise Call
  end
end

module Comp (M1 : UNIT) (M2 : UNIT with type Op.t = M1.Op.t) = struct
  exception Call

  module Op : OPERATOR with type t = M1.Op.t = struct
    type t = M1.Op.t
    let to_string o =
      try M1.Op.to_string o
      with M1.Call ->
        try M2.Op.to_string o
        with M2.Call ->
          raise Call

    let arity o =
      try M1.Op.arity o
      with M1.Call ->
        try M2.Op.arity o
        with M2.Call ->
          raise Call
  end

  module Typing (Syntax : ABT with type Operator.t = Op.t) = struct
    module Typing1 = M1.Typing(Syntax)
    module Typing2 = M2.Typing(Syntax)

    let judge j =
      try Typing1.judge j
      with M1.Call ->
        try Typing2.judge j
        with M2.Call ->
          raise Call
  end
end

module Kernel = struct
  module Op = struct
    type t = ..
  end
end

module Pi' = Pi (Kernel)
module Sg' = Sg (Kernel)
module One' = One (Kernel)

module Lang = Comp (Comp (Pi') (Sg')) (One')

open Pi'
open Sg'
open One'

module L = struct
  include Lang.Op

  module Syn = Abt(Var)(Lang.Op)
  module Ty = Lang.Typing(Syn)
  module SynKit = Abt_Util(Syn)
  include Ty
end

open L
open L.SynKit

let welp = L.judge (Chk (AX $$ [], UNIT $$ []))
