module Ex1.Ex1 exposing (..)

import Ex1.Input exposing (problemInput)

import Set

type alias Frequency = Int
type alias DeltaFrequency = Int

parseInput : String -> List DeltaFrequency
parseInput =
  String.lines "\n"
  >> List.map String.toInt
  >> List.map (Maybe.withDefault 0)

part1answer = parseInput problemInput |> List.sum

findFirstMatch : List DeltaFrequency -> List DeltaFrequency -> Set.Set Frequency -> Frequency -> Frequency
findFirstMatch input remaining seen total =
    case remaining of
      [] -> findFirstMatch input input seen total
      x::xs ->
        let
          nextTotal = total + x
        in
          if Set.member nextTotal seen
            then nextTotal
            else findFirstMatch input xs (Set.insert total seen) nextTotal

part2answer =
  let
    input = parseInput problemInput
  in
    findFirstMatch input input Set.empty 0
