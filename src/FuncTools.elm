module FuncTools exposing (permutations, combinations, pairEq, frequencies)

import Dict exposing (Dict)

permutations : List a -> List (a, a)
permutations l =
  let
    step a b =
      case a of
        [] -> []
        x::xs -> List.map (\y -> (x, y)) b ++ step xs b
  in
    step l l

combinations : List a -> List (a, a)
combinations l =
  let
    step a b =
      case a of
        [] -> []
        x::xs -> List.map (\y -> (x, y)) (List.drop 1 b) ++ step xs (List.drop 1 b)
  in
    step l l

pairEq : (a, a) -> Bool
pairEq p = let (a, b) = p in a == b

addToFrequencies : comparable -> Dict comparable Int -> Dict comparable Int
addToFrequencies x acc = Dict.update x (\mv -> Just ((Maybe.withDefault 0 mv) + 1)) acc

frequencies : List comparable -> Dict comparable Int
frequencies = List.foldl addToFrequencies Dict.empty
