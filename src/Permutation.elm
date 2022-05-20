module Permutation exposing (Permutation, map, inverse, fromArray, fromList, compose, identity, swap)

import Array exposing (Array)
import Tuple exposing (pair, first, second)

-- A permutation is stored as an array of indices, where the `i`th entry gives the position that `i` is mapped to.
type alias Permutation = Array Int

-- Map a position under a given permutation
map : Permutation -> Int -> Int
map p i = Array.get i p |> Maybe.withDefault i

inverse : Permutation -> Permutation
inverse = 
    Array.indexedMap Tuple.pair >> Array.foldl (\(i,j) -> Array.set j i) Array.empty

fromArray : Array Int -> Permutation
fromArray = Basics.identity

fromList : List Int -> Permutation
fromList = Array.fromList

compose : Permutation -> Permutation -> Permutation
compose a b =
    let
        len = max (Array.length a) (Array.length b)
    in
        List.range 0 (len-1) |> Array.fromList |> Array.map (map a >> map b)

swap : Int -> Int -> Permutation
swap a b =
    List.range 0 (max a b) |> Array.fromList |> Array.map (\x -> if x==a then b else if x==b then a else x)

identity = Array.empty
