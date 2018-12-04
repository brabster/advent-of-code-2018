module Ex3.Ex3 exposing (..)

import Ex3.Input exposing (..)
import FuncTools
import Set

type alias ClaimId = String
type alias Point = { x: Int, y: Int }
type alias TopCorner = Point
type alias Area = Point
type alias Claim =
  { id: ClaimId
    , topCorner : TopCorner
    , area : Area
  }

tupleToClaim tuple =
  case tuple of
    (id, [left, top], [width, height]) -> Just (Claim id (Point left top) (Point width height))
    _ -> Nothing

parseClaimSpec =
  String.split " "
  >> (\list ->
    case list of
      [claimNumber,_,topLeft,rectangle] -> Just (
        String.replace "#" "" claimNumber
        , String.split "," (String.replace ":" "" topLeft) |> List.map (String.toInt >> Maybe.withDefault 0)
        , String.split "x" rectangle |> List.map (String.toInt >> Maybe.withDefault 0))
      _ -> Nothing)
  >> (\tuple ->
    case tuple of
      Just t -> tupleToClaim t
      Nothing -> Nothing)

parseInput =
  String.lines
  >> List.map parseClaimSpec
  >> List.filterMap identity

type alias Fabric = List (List Int)
emptyFabric = List.repeat 1000 (List.repeat 1000 0)

updateRow : Claim -> List Int -> List Int
updateRow claim row =
  List.indexedMap (\cellIdx cell -> if cellIdx >= claim.topCorner.x && cellIdx < claim.topCorner.x + claim.area.x then cell + 1 else cell) row

applyClaim : Claim -> Fabric -> Fabric
applyClaim claim fabric =
    List.indexedMap (\idx row -> if idx >= claim.topCorner.y && idx < claim.topCorner.y + claim.area.y then updateRow claim row else row) fabric

part1answer = parseInput problemInput
  |> List.foldl applyClaim emptyFabric
  |> List.map (List.filter (\x -> x >= 2))
  |> List.map List.length
  |> List.sum
