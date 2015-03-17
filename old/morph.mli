module Morph : functor (KM : Kleisli.KLEISLI) -> sig
  val morph : ('s -> 't KM.t) -> ('s, 't, 'x) General.t -> 'x KM.t
end
