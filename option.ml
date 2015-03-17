type 'a t =
  | Some of 'a
  | None

type 'a option = 'a t
module OptionK : Kleisli.KLEISLI with type 'a t = 'a t = struct
  type 'a t = 'a option

  let return x =
    Some x

  let bind mx f =
    match mx with
    | Some x -> f x
    | None -> None
end
