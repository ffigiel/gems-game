module Array2d exposing
    ( Array2d
    , any
    , empty
    , foldl
    , fromList
    , get
    , indexedMap
    , map
    , set
    , size
    , toList
    , transpose
    )

import Array exposing (Array)
import List.Extra


type alias Array2d a =
    Array (Array a)


map : (a -> b) -> Array2d a -> Array2d b
map f =
    Array.map (Array.map f)


any : (( Int, Int, a ) -> Bool) -> Array2d a -> Bool
any f array =
    array
        |> indexedMap (\x y a -> ( x, y, a ))
        |> toList
        |> List.concat
        |> List.any f


indexedMap : (Int -> Int -> a -> b) -> Array2d a -> Array2d b
indexedMap f =
    Array.indexedMap (\y -> Array.indexedMap (\x -> f x y))


get : Int -> Int -> Array2d a -> Maybe a
get x y =
    Array.get y
        >> Maybe.andThen (Array.get x)


fromList : List (List a) -> Array2d a
fromList =
    List.map Array.fromList
        >> Array.fromList


toList : Array2d a -> List (List a)
toList =
    Array.map Array.toList
        >> Array.toList


empty : Array2d a
empty =
    Array.empty


set : Int -> Int -> a -> Array2d a -> Array2d a
set x y a arr =
    case Array.get y arr of
        Just row ->
            let
                newRow =
                    Array.set x a row
            in
            Array.set y newRow arr

        Nothing ->
            arr


size : Array2d a -> ( Int, Int )
size arr =
    let
        width =
            case Array.get 0 arr of
                Just row ->
                    Array.length row

                Nothing ->
                    0

        height =
            Array.length arr
    in
    ( width, height )


transpose : Array2d a -> Array2d a
transpose =
    toList
        >> List.Extra.transpose
        >> fromList


foldl : (a -> b -> b) -> b -> Array2d a -> b
foldl acc =
    Array.foldl (\row x -> Array.foldl acc x row)
