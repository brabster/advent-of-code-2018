module Ex5.Ex5 exposing (..)

import Ex5.Input exposing (..)

import Array exposing (Array)
import Regex
import Debug

reactsWith : Char -> Char -> Bool
reactsWith a b = a /= b && Char.toUpper a == Char.toUpper b

reactStep : Array Char -> Int -> Array Char
reactStep chars index =
  if index > Array.length chars
    then chars
    else
      let
        reacts = Maybe.map2 reactsWith (Array.get index chars) (Array.get (index + 1) chars)
      in
        if Maybe.withDefault False reacts
          then Array.append (Array.slice 0 index chars) (Array.slice (index + 2) (Array.length chars) chars)
          else reactStep chars (index + 1)


fullyReact : Array Char -> Array Char
fullyReact chars =
  let
    step1 = reactStep chars 0
    step2 = reactStep step1 0
    --dummy = Debug.log "step1" (Array.length step1)
    --dummy2 = Debug.log "step2" (Array.length step2)
  in
    if step1 == step2
      then step1
      else fullyReact step2

reactString = String.toList >> Array.fromList >> fullyReact >> Array.length

part1answer = reactString problemInput

replaceRegex string re =
  Regex.replace (Maybe.withDefault Regex.never (Regex.fromString re)) (\_ -> "") string

typesToTry = String.toList "abcdefghijklmnopqrstuvwxyz"
  |> List.map (
    String.fromChar
    >> (\s -> ["[", s, String.toUpper s, "]"])
    >> String.join "")

part2answer =
  List.map
    ((replaceRegex problemInput)
    >> reactString) typesToTry
