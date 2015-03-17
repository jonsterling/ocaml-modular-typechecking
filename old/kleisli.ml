module type KLEISLI = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type KLEISLI_KIT = sig
  include KLEISLI
  val compose : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
end

module KleisliKit(K : KLEISLI) : KLEISLI_KIT = struct
  include K
  let compose f g x = bind (g x) f
end

module Infix(K : KLEISLI) = struct
  open K
  let (>>=) = bind
end
