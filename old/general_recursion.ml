open General
open Option
open Morph

let expand (type a) (type b) (f : (a, b) General.fn) =
  let module GeneralK = GeneralK(struct type s = a type t = b end) in
  let module Morph = Morph(GeneralK) in
  Morph.morph f

let already x =
  let module Morph = Morph(OptionK) in
  Morph.morph (fun _ -> None) x

let rec engine f n g =
  if (n <= 0) then g else
    engine f (n - 1) (expand f g)

let petrol f n s =
  already (engine f n (f s))
