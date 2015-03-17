type 'a t =
  | Some of 'a
  | None

module OptionK : Kleisli.KLEISLI with type 'x t = 'x t
