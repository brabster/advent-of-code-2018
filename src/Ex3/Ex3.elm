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

inIntRange : Int -> Int -> Int -> Bool
inIntRange lower upper test = lower < test && test < upper

overlaps : Claim -> Claim -> Bool
overlaps claim1 claim2 =
  let
    inXRange claim = inIntRange claim.topCorner.x (claim.topCorner.x + claim.area.x)
    inYRange claim = inIntRange claim.topCorner.y (claim.topCorner.y + claim.area.y)
  in
    (inXRange claim1 claim2.topCorner.x || inXRange claim1 (claim2.topCorner.x + claim2.area.x))
    && (inYRange claim1 claim2.topCorner.y || inYRange claim1 (claim2.topCorner.y + claim2.area.y))

findOverlapping : (ClaimId, ClaimId, Bool) -> Set.Set ClaimId -> Set.Set ClaimId
findOverlapping (claimId1, claimId2, doesOverlap) overlapping =
  if doesOverlap then Set.insert claimId1 overlapping |> Set.insert claimId2 else overlapping

part2answer =
  let
    claims = parseInput problemInput
  in
    claims
    |> FuncTools.combinations
    |> List.filter (\(c1, c2) -> c1.id == "59" || c2.id == "59")
    |> List.map (\(claim1, claim2) -> (claim1.id, claim2.id, overlaps claim1 claim2))
    --|> List.foldl findOverlapping Set.empty
    --|> Set.diff (Set.fromList (List.map .id claims))
