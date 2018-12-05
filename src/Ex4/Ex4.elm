module Ex4.Ex4 exposing (..)

import Ex4.Input exposing (..)
import Dict
import FuncTools

type alias GuardId = String
type Activity = Wake | Sleep | StartShift GuardId

type alias Date = String
type alias Time = String
type alias TimestampedActivity = (Date, Time, Activity)

parseGuardId : String -> Maybe GuardId
parseGuardId activity =
  case String.split " " activity of
    [_, guardId, _, _] -> Just guardId
    _ -> Nothing

parseActivity : String -> Maybe Activity
parseActivity activity =
  case List.head (String.words activity) of
    Just "wakes" -> Just Wake
    Just "falls" -> Just Sleep
    Just "Guard" -> Maybe.map StartShift (parseGuardId activity)
    _ -> Nothing

parseLine line =
  let
    date = String.slice 1 11 line
    time = String.slice 12 17 line
    activity = parseActivity (String.dropLeft 19 line)
  in
    Maybe.map (\a -> (date, time, a)) activity

compareTimestampedActivity : TimestampedActivity -> TimestampedActivity -> Order
compareTimestampedActivity (d1, t1, _) (d2, t2, _) = if d1 == d2 then compare t1 t2 else compare d1 d2

parseInput =
  String.lines
  >> List.filterMap parseLine
  >> List.sortWith compareTimestampedActivity

accumulateStepUpdate : TimestampedActivity -> (Maybe (List TimestampedActivity) -> Maybe (List TimestampedActivity))
accumulateStepUpdate next maybeActs =
  case maybeActs of
    Just acts -> Just (next :: acts)
    Nothing -> Just [next]

type alias AccStepAccumulator = (Maybe GuardId, Dict.Dict GuardId (List TimestampedActivity))
accumulateStep : TimestampedActivity -> AccStepAccumulator -> AccStepAccumulator
accumulateStep tsa acc =
  let
    (_, _, activity) = tsa
    (maybeLastGuardId, dict) = acc
  in
    case activity of
      StartShift guardId -> (Just guardId, dict)
      _ -> (maybeLastGuardId, Dict.update (Maybe.withDefault "#ERROR" maybeLastGuardId) (accumulateStepUpdate tsa) dict)

listToPairs : List a -> List (a, a)
listToPairs list =
  case list of
    [] -> []
    [x] -> []
    x::y::xs -> (x, y) :: listToPairs xs

minsAsleep : (Int, Int) -> Int
minsAsleep (x, y) = y - x

sleepsByGuard input = input
  |> List.foldl accumulateStep (Nothing, Dict.empty)
  |> Tuple.second
  |> Dict.toList
  |> List.map (\(guardId, activities) -> (guardId, List.reverse activities
    |> List.map ((\(_,time,_) -> time)
      >> String.split ":"
      >> List.drop 1
      >> List.head
      >> Maybe.withDefault "00"
      >> String.toInt
      >> Maybe.withDefault 0)))
  |> Dict.fromList

guardMinuteAsleepFrequencies guardSleeps = guardSleeps
  |> listToPairs
  |> List.map (\(a, b) -> List.range a b)
  |> List.concat
  |> FuncTools.frequencies

maxValuedEntry = Dict.foldl (\k v (k1, v1) -> if v > v1 then (k, v) else (k1, v1))

answerHash : Int -> GuardId -> Int
answerHash minute guardId = minute * (String.replace "#" "" guardId |> String.toInt |> Maybe.withDefault 0)

part1answer =
  let
    input = parseInput problemInput
    guardSleeps = sleepsByGuard input
    guardTotalMinsAsleep = guardSleeps
      |> Dict.map (\_ v -> v
      |> (listToPairs
        >> List.map minsAsleep
        >> List.sum))
    guardMostAsleep = guardTotalMinsAsleep
      |> maxValuedEntry ("", 0)
      |> Tuple.first
    guardMostAsleepSleeps = guardMinuteAsleepFrequencies (Dict.get guardMostAsleep guardSleeps |> Maybe.withDefault [])
      |> maxValuedEntry (-1, -1)
  in
    answerHash (Tuple.first guardMostAsleepSleeps) guardMostAsleep

part2answer =
  let
    input = parseInput problemInput
    guardSleeps = sleepsByGuard input
  in
    guardSleeps
    |> Dict.map (\_ sleeps ->
      guardMinuteAsleepFrequencies sleeps
      |> maxValuedEntry (-1, -1))
    |> Dict.foldl (\k (m, v) (k1, (m1, v1)) -> if v > v1 then (k, (m, v)) else (k1, (m1, v1))) ("", (-1, -1))
    |> (\(guardId, (minute, _)) -> answerHash minute guardId)
