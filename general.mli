type ('s, 't, 'x) t =
  | Now of 'x
  | Later of 's * ('t -> ('s, 't, 'x) t)

type ('s, 't) fn = 's -> ('s, 't, 't) t

val fold : ('x -> 'y) -> ('s * ('t -> 'y) -> 'y) -> ('s, 't, 'x) t -> 'y
val bind : ('s, 't, 'x) t -> ('x -> ('s, 't, 'y) t) -> ('s, 't, 'y) t
val call : 's  -> ('s, 't, 't) t

module Infix : sig
  val (!!) : 'x -> ('s, 't, 'x) t
  val (^?) : 's -> ('t -> ('s, 't, 'x) t) -> ('s, 't, 'x) t
  val (>>=) : ('s, 't, 'x) t -> ('x -> ('s, 't, 'y) t) -> ('s, 't, 'y) t
end

module GeneralK
  : functor (M : sig type s type t end)
  -> Kleisli.KLEISLI with type 'x t = (M.s, M.t, 'x) t
