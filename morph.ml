open Kleisli
open General

module Morph (KM : KLEISLI) = struct
  module KMI = Kleisli.Infix(KM)
  open KMI
  
  let morph h =
    fold KM.return (fun (s, t) -> h s >>= t)
end
