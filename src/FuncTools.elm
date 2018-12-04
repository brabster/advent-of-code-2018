module FuncTools exposing (permutations, pairEq)

permutations : List a -> List (a, a)
permutations l =
  let
    permutationsInternal a b =
      case a of
        [] -> []
        x::xs -> List.map (\y -> (x, y)) b ++ permutationsInternal xs b
  in
    permutationsInternal l l

pairEq : (a, a) -> Bool
pairEq p = let (a, b) = p in a == b
