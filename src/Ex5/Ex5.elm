module Ex5.Ex5 exposing (..)

import Ex5.Input exposing (..)

import Debug

reactsWith : Char -> Char -> Bool
reactsWith a b = a /= b && Char.toUpper a == Char.toUpper b

reactStep : List Char -> List Char -> List Char
reactStep results list =
  case list of
    [] -> List.reverse results
    [a] -> a :: results
    a::b::cs ->
      let
        nextRest = if reactsWith a b
          then cs
          else b :: cs
        nextResults = if reactsWith a b
          then results
          else a :: results
      in
        reactStep nextResults nextRest

fullyReact : List Char -> List Char
fullyReact list =
  let
    step1 = reactStep [] list
    step2 = reactStep [] step1
    dummy = Debug.log "step1" (List.length step1)
    dummy2 = Debug.log "step2" (List.length step2)
  in
    if step1 == step2
      then step1
      else fullyReact step2

part1answer = problemInput
  |> String.toList
  |> fullyReact
  |> List.length
