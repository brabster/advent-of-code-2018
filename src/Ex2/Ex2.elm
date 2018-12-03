module Ex2.Ex2 exposing (..)

import Ex2.Input exposing (problemInput)
import Dict exposing (..)
import Debug exposing (..)

type alias BoxId = Char
type alias Count = Int

addToFrequencies : BoxId -> Dict BoxId Count -> Dict BoxId Count
addToFrequencies x acc = update x (\mv -> Just ((Maybe.withDefault 0 mv) + 1)) acc

frequencies : String -> Dict BoxId Count
frequencies = String.foldl addToFrequencies empty

hasValue : Count -> Dict BoxId Count -> Bool
hasValue wanted dict = List.any (\v -> v == wanted) (values dict)

countHasValue v dicts = List.map frequencies dicts |> List.filter (hasValue v) |> List.length

part1answer : Int
part1answer =
  let
    parsed = String.lines problemInput
  in
    countHasValue 2 parsed * countHasValue 3 parsed

zip : List a -> List b -> List (a, b)
zip a b =
  case a of
    [] -> []
    x::xs -> case b of
      [] -> []
      y::ys ->  (x, y) :: zip xs ys

pairEq : (a, a) -> Bool
pairEq p = let (a, b) = p in a == b

editDistance : List a -> List a -> Int
editDistance xs ys =
  zip xs ys
  |> List.map (\p -> if pairEq p then 0 else 1)
  |> List.sum

permutations : List a -> List (a, a)
permutations l =
  let
    permutationsInternal a b =
      case a of
        [] -> []
        x::xs -> List.map (\y -> (x, y)) b ++ permutationsInternal xs b
  in
    permutationsInternal l l


part2answer =
  String.lines problemInput
  |> List.map String.toList
  |> permutations
  |> List.filter (\p -> let (a, b) = p in editDistance a b == 1)
  |> List.map (\p -> let (a, b) = p in zip a b)
  |> List.map (List.filter pairEq)
  |> List.map (List.map Tuple.first)
  |> List.map (List.map String.fromChar)
  |> List.map (String.join "")
  |> List.head
