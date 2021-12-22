module Board exposing
    ( Board
    , Chain
    , Piece(..)
    , chainOfSameColor
    , chainScore
    , generator
    , isGameOver
    , isValid
    , minChain
    , myHighScore
    , numCols
    , numRows
    , piecesQueueGenerator
    , queueSize
    , removePieces
    , validChain
    )

import Array
import Array2d exposing (Array2d)
import Dict exposing (Dict)
import Random exposing (Generator)


myHighScore : Int
myHighScore =
    6969


numCols : Int
numCols =
    6


numRows : Int
numRows =
    8


minChain : Int
minChain =
    3


queueSize : Int
queueSize =
    numCols * numRows


generator : Generator Board
generator =
    pieceGenerator
        |> Random.list numCols
        |> Random.list numRows
        |> Random.map Array2d.fromList
        |> Random.andThen
            (\b ->
                if isValid b then
                    Random.constant b

                else
                    generator
            )


piecesQueueGenerator : Generator (List Piece)
piecesQueueGenerator =
    Random.list queueSize pieceGenerator


pieceGenerator : Generator Piece
pieceGenerator =
    Random.uniform Red pieces


type alias Board =
    Array2d Piece


type Piece
    = Red
    | Orange
    | Yellow
    | Green
    | Cyan
    | Blue
    | Purple
    | White


pieces : List Piece
pieces =
    [ Red
    , Orange
    , Yellow
    , Green
    , Cyan
    , Blue
    , Purple
    , White
    ]


type alias BoardSearch =
    Array2d { piece : Piece, visited : Bool }


type alias Chain =
    Dict ( Int, Int ) Piece


validChain : Piece -> ( Int, Int ) -> Board -> Chain
validChain piece ( x, y ) board =
    let
        chain =
            chainOfSameColor piece ( x, y ) board
    in
    if Dict.size chain < minChain then
        Dict.empty

    else
        chain


chainOfSameColor : Piece -> ( Int, Int ) -> Board -> Chain
chainOfSameColor piece ( x, y ) board =
    let
        boardSearch : BoardSearch
        boardSearch =
            board
                |> Array2d.map
                    (\p ->
                        { piece = p
                        , visited = False
                        }
                    )

        ( _, result ) =
            chainOfSameColorHelper piece ( x, y ) ( boardSearch, Dict.empty )
    in
    result


chainOfSameColorHelper :
    Piece
    -> ( Int, Int )
    -> ( BoardSearch, Chain )
    -> ( BoardSearch, Chain )
chainOfSameColorHelper piece ( x, y ) ( boardSearch, results ) =
    let
        match =
            boardSearch
                |> Array2d.get x y
                |> Maybe.andThen
                    (\t ->
                        if not t.visited && t.piece == piece then
                            Just t.piece

                        else
                            Nothing
                    )

        newBoardSearch =
            boardSearch
                |> Array2d.indexedMap
                    (\tx ty t ->
                        if ty == y && tx == x then
                            { t | visited = True }

                        else
                            t
                    )
    in
    case match of
        Just p ->
            ( newBoardSearch, Dict.insert ( x, y ) p results )
                |> chainOfSameColorHelper piece ( x + 1, y )
                |> chainOfSameColorHelper piece ( x - 1, y )
                |> chainOfSameColorHelper piece ( x, y + 1 )
                |> chainOfSameColorHelper piece ( x, y - 1 )

        Nothing ->
            ( newBoardSearch, results )


chainScore : Chain -> Int
chainScore chain =
    let
        n =
            toFloat <| Dict.size chain
    in
    logBase (toFloat minChain) n
        |> (*) (n * 10)
        |> round


removePieces : Chain -> List Piece -> Board -> ( Board, List Piece, Dict ( Int, Int ) Int )
removePieces removedPieces piecesQueue board =
    let
        ( _, colLength ) =
            Array2d.size board

        withPiecesRemoved : Array2d (Maybe Piece)
        withPiecesRemoved =
            board
                |> Array2d.indexedMap
                    (\x y piece ->
                        if Dict.member ( x, y ) removedPieces then
                            Nothing

                        else
                            Just piece
                    )

        findGaps : List (Maybe a) -> List Int
        findGaps =
            -- given a board column, calculate from what distance the pieces will fall
            -- should return
            -- [x, _, _, x, _] becomes
            -- [0, 2, 2, 3, 3]
            --  ^-bottom row    ^-top row
            List.foldl
                (\el ( results, counter ) ->
                    case el of
                        Just _ ->
                            ( counter :: results, counter )

                        Nothing ->
                            ( results, counter + 1 )
                )
                ( [], 0 )
                >> (\( results, counter ) ->
                        List.repeat counter counter ++ results
                   )
                >> List.reverse

        ( withPiecesShifted, fallingPieces ) =
            let
                cols =
                    Array2d.transpose withPiecesRemoved

                shifted =
                    cols
                        |> Array.map
                            (\col ->
                                let
                                    filteredCol =
                                        Array.filter (\m -> m /= Nothing) col

                                    remainder =
                                        Array.repeat (colLength - Array.length filteredCol) Nothing
                                in
                                Array.append remainder filteredCol
                            )
                        |> Array2d.transpose

                ( _, falling ) =
                    cols
                        |> Array.foldl
                            (\col ( x, f ) ->
                                col
                                    |> Array.toList
                                    |> List.reverse
                                    |> findGaps
                                    |> List.reverse
                                    |> List.foldl
                                        (\n ( y, f2 ) ->
                                            ( y + 1
                                            , if n == 0 then
                                                f2

                                              else
                                                ( ( x, y ), n ) :: f2
                                            )
                                        )
                                        ( 0, f )
                                    |> Tuple.mapFirst (always (x + 1))
                            )
                            ( 0, [] )
            in
            ( shifted, falling )

        blankCoords =
            withPiecesShifted
                |> Array2d.indexedMap
                    (\x y p ->
                        if p == Nothing then
                            Just ( x, y )

                        else
                            Nothing
                    )
                |> Array2d.foldl
                    (\el coords ->
                        case el of
                            Just xy ->
                                xy :: coords

                            Nothing ->
                                coords
                    )
                    []

        ( withNewPieces, newQueue ) =
            List.foldl
                (\( x, y ) ( b, q ) ->
                    let
                        newB =
                            Array2d.set x y (List.head q) b

                        newQ =
                            List.drop 1 q
                    in
                    ( newB, newQ )
                )
                ( withPiecesShifted, piecesQueue )
                blankCoords
    in
    ( Array2d.map (Maybe.withDefault Red) withNewPieces, newQueue, Dict.fromList fallingPieces )


isValid : Board -> Bool
isValid board =
    board
        |> Array2d.indexedMap (\x y piece -> ( x, y, piece ))
        |> Array2d.toList
        |> List.concat
        |> List.all
            (\( x, y, piece ) ->
                let
                    chain =
                        chainOfSameColor piece ( x, y ) board
                in
                Dict.size chain < minChain
            )


isGameOver : Board -> Bool
isGameOver _ =
    -- TODO
    False
