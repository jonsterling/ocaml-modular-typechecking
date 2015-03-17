type ('s, 't, 'x) t =
  | Now of 'x
  | Later of 's * ('t -> ('s, 't, 'x) t)

type ('s, 't) fn = 's -> ('s, 't, 't) t

let rec fold r c g =
  match g with
  | Now x -> r x
  | Later (s , k) -> c (s, fun t -> fold r c (k t))

let bind g k =
  fold k (fun (s, t) -> Later (s, t)) g

module Infix = struct
  let (!!) x = Now x
  let (>>=) g k = bind g k
  let (^?) s t = Later (s, t)
end

open Infix
  
let call s =
  s ^? (!!) 

type ('s, 't, 'x) general = ('s, 't, 'x) t

module GeneralK(M : sig type s type t end)
  : Kleisli.KLEISLI with type 'x t = (M.s, M.t, 'x) t =
struct
  type 'x t = (M.s, M.t, 'x) general
  let return = (!!)
  let bind = (>>=)
end
