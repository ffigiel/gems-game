port module Main exposing (main)

import Array
import Array2d
import Board exposing (Board, Piece(..))
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Ease
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy as HL
import Json.Decode as JD
import Process
import Random
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Task



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port saveHighScore : Int -> Cmd msg


port vibrate : () -> Cmd msg


port logError : String -> Cmd msg



-- MODEL


type alias Flags =
    JD.Value


type alias Model =
    { board : Board
    , gameBbox : Bbox
    , heldPiece : Maybe HeldPiece
    , time : Float
    , isGameOver : Bool
    , piecesQueue : List Piece
    , score : Int
    , highScore : Int
    , isNewHighScore : Bool
    , removedPieces : RemovedPieces
    , fallingPieces : FallingPieces
    }


type alias Bbox =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type alias HeldPiece =
    { piece : Piece

    -- coordinates of the grabbed piece
    , pos : ( Int, Int )

    -- click/tap xy coordinates
    , startPoint : ( Float, Float )

    -- <0, 1> range reflecting the current position on the board
    , gamePos : ( Float, Float )
    }


type alias RemovedPieces =
    Dict ( Int, Int ) RemovedPiecesData


type alias RemovedPiecesData =
    { start : Float
    , piece : Piece
    }


type alias FallingPieces =
    Dict ( Int, Int ) FallingPiecesData


type alias FallingPiecesData =
    { start : Float
    , duration : Float
    , distance : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { board = Array.empty
            , gameBbox = Bbox 0 0 100 100
            , heldPiece = Nothing
            , time = 0
            , isGameOver = False
            , piecesQueue = []
            , score = 0
            , highScore = highScore
            , isNewHighScore = False
            , removedPieces = Dict.empty
            , fallingPieces = Dict.empty
            }

        highScore =
            JD.decodeValue (JD.field "highScore" JD.string) flags
                |> Result.toMaybe
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0

        cmd =
            Cmd.batch
                [ generateBoardCmd
                , getGameBboxCmd
                ]
    in
    ( model, cmd )


generateBoardCmd : Cmd Msg
generateBoardCmd =
    Random.map2 (\board piecesQueue -> { board = board, piecesQueue = piecesQueue })
        Board.generator
        Board.piecesQueueGenerator
        |> Random.generate Init


getGameBboxCmd : Cmd Msg
getGameBboxCmd =
    Browser.Dom.getElement "gameView"
        |> Task.attempt
            (\res ->
                res
                    |> Result.map
                        (\e ->
                            { x = e.element.x
                            , y = e.element.y
                            , w = e.element.width
                            , h = e.element.height
                            }
                        )
                    |> GotGameBbox
            )



-- UPDATE


type Msg
    = Init { board : Board, piecesQueue : List Piece }
    | ResizedWindow
    | GotGameBbox (Result Browser.Dom.Error Bbox)
    | Tick Float
    | GrabbedPiece Piece ( Int, Int ) ( Float, Float )
    | MovedPiece ( Float, Float )
    | DroppedPiece
    | GotPiecesQueue (List Piece)
    | RemoveAnimationState Int
    | GameOver
    | PlayAgainClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init { board, piecesQueue } ->
            ( { model
                | board = board
                , piecesQueue = model.piecesQueue ++ piecesQueue
              }
            , if Board.isGameOver board then
                generateBoardCmd

              else
                Cmd.none
            )

        ResizedWindow ->
            ( { model | heldPiece = Nothing }
            , getGameBboxCmd
            )

        GotGameBbox (Ok bbox) ->
            ( { model | gameBbox = bbox }
            , Cmd.none
            )

        GotGameBbox (Err (Browser.Dom.NotFound err)) ->
            ( model
            , logError ("GotGameBbox Err: " ++ err)
            )

        Tick d ->
            ( { model | time = model.time + d }, Cmd.none )

        GrabbedPiece piece ( x, y ) mouseXY ->
            ( { model
                | heldPiece =
                    Just
                        { piece = piece
                        , pos = ( x, y )
                        , startPoint = mouseToGamePosition model.gameBbox mouseXY
                        , gamePos = mouseToGamePosition model.gameBbox mouseXY
                        }
              }
            , Cmd.none
            )

        MovedPiece mouseXY ->
            case model.heldPiece of
                Just hp ->
                    ( { model
                        | heldPiece =
                            Just { hp | gamePos = mouseToGamePosition model.gameBbox mouseXY }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DroppedPiece ->
            case model.heldPiece of
                Nothing ->
                    ( model, Cmd.none )

                Just hp ->
                    if hp.startPoint == hp.gamePos then
                        ( { model | heldPiece = Nothing }
                        , Cmd.none
                        )

                    else
                        let
                            ( x, y ) =
                                hp.pos

                            dir =
                                getDirection hp.startPoint hp.gamePos

                            ( newX, newY ) =
                                case dir of
                                    Up ->
                                        ( x, y + 1 )

                                    Down ->
                                        ( x, y - 1 )

                                    Left ->
                                        ( x - 1, y )

                                    Right ->
                                        ( x + 1, y )

                            switchedPiece =
                                Array2d.get newX newY model.board

                            ( newBoard, chain ) =
                                case switchedPiece of
                                    Just sp ->
                                        let
                                            b =
                                                model.board
                                                    |> Board.swapPieces ( x, y ) ( newX, newY )

                                            c =
                                                Dict.union
                                                    (Board.validChain sp ( x, y ) b)
                                                    (Board.validChain hp.piece ( newX, newY ) b)
                                        in
                                        ( b, c )

                                    Nothing ->
                                        ( model.board, Dict.empty )
                        in
                        { model | heldPiece = Nothing }
                            |> (\m ->
                                    case Dict.size chain of
                                        0 ->
                                            ( m, Cmd.none )

                                        _ ->
                                            { m | board = newBoard }
                                                |> handleValidMove chain
                               )

        GotPiecesQueue queue ->
            ( { model | piecesQueue = model.piecesQueue ++ queue }, Cmd.none )

        RemoveAnimationState score ->
            ( if score == model.score then
                { model
                    | removedPieces = Dict.empty
                    , fallingPieces = Dict.empty
                }

              else
                -- player clicked during the transition and a new animation started playing
                model
            , Cmd.none
            )

        GameOver ->
            let
                highScore =
                    max model.highScore model.score

                isNewHighScore =
                    model.score > model.highScore
            in
            ( { model
                | isGameOver = True
                , highScore = highScore
                , isNewHighScore = isNewHighScore
              }
            , if isNewHighScore then
                saveHighScore highScore

              else
                Cmd.none
            )

        PlayAgainClicked ->
            ( { model
                | isGameOver = False
                , score = 0
                , isNewHighScore = False
                , removedPieces = Dict.empty
                , fallingPieces = Dict.empty
              }
            , generateBoardCmd
            )


mouseToGamePosition : Bbox -> ( Float, Float ) -> ( Float, Float )
mouseToGamePosition gameBbox ( mouseX, mouseY ) =
    let
        xPercent =
            (mouseX - gameBbox.x) / gameBbox.w

        yPercent =
            (mouseY - gameBbox.y) / gameBbox.h
    in
    ( clamp 0 1 xPercent, clamp 0 1 yPercent )


handleValidMove : Board.Chain -> Model -> ( Model, Cmd Msg )
handleValidMove chain model =
    let
        ( newBoard, newPiecesQueue, fallingPieces ) =
            Board.removePieces chain
                model.piecesQueue
                model.board

        newScore =
            model.score + Board.chainScore chain

        removedPieces =
            Dict.map
                (\_ p ->
                    { start = model.time, piece = p }
                )
                chain

        maxDistance =
            fallingPieces
                |> Dict.foldl (\_ distance acc -> max distance acc) 0

        newFallingPieces =
            fallingPieces
                |> Dict.map
                    (\_ distance ->
                        { start = model.time
                        , duration = calcFallingAnimationDuration distance
                        , distance = distance
                        }
                    )

        animationsDuration =
            calcFallingAnimationDuration maxDistance
    in
    ( { model
        | score = newScore
        , removedPieces = removedPieces
        , board = newBoard
        , fallingPieces = newFallingPieces
        , piecesQueue = newPiecesQueue
      }
    , Cmd.batch
        [ refillPiecesQueue newPiecesQueue
        , Task.perform
            (\_ -> RemoveAnimationState newScore)
            (Process.sleep animationsDuration)
        , if Board.isGameOver newBoard then
            -- show game over screen with a small delay after click
            Task.perform
                (\_ -> GameOver)
                (Process.sleep gameOverScreenDelay)

          else
            Cmd.none
        , vibrate ()
        ]
    )


refillPiecesQueue : List Piece -> Cmd Msg
refillPiecesQueue queue =
    if List.length queue < Board.queueSize then
        Random.generate GotPiecesQueue Board.piecesQueueGenerator

    else
        Cmd.none


calcFallingAnimationDuration : Int -> Float
calcFallingAnimationDuration distance =
    -- slightly longer falling animation for higher distances
    logBase (toFloat Board.minChain) (toFloat <| Board.minChain + distance - 1)
        * fallingAnimationBaseDuration


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize (\_ _ -> ResizedWindow)
        , case model.heldPiece of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (JD.map MovedPiece mouseEventPositionDecoder)
                    , Browser.Events.onMouseUp (JD.succeed DroppedPiece)
                    ]

            Nothing ->
                Sub.none
        ]


mouseEventPositionDecoder : JD.Decoder ( Float, Float )
mouseEventPositionDecoder =
    JD.map2 Tuple.pair
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)



-- VIEW


animationScale : number
animationScale =
    -- for testing
    1


removingAnimationDuration : number
removingAnimationDuration =
    400 * animationScale


fallingAnimationBaseDuration : number
fallingAnimationBaseDuration =
    400 * animationScale


gameOverScreenDelay : number
gameOverScreenDelay =
    200 * animationScale


calcAnimationProgress : Float -> Float -> Float -> Float
calcAnimationProgress tNow tStart duration =
    ((tNow - tStart) / duration)
        |> clamp 0 1


fallingAnimationEasing : Int -> Ease.Easing
fallingAnimationEasing distance =
    Ease.inOut Ease.inBack (easeOutBackDampened distance)


easeOutBackDampened : Int -> Ease.Easing
easeOutBackDampened dampening t =
    -- dampening prevents the pieces from overlapping
    let
        y =
            Ease.outBack t

        fd =
            toFloat dampening
    in
    if y > 1 then
        (y + fd - 1) / fd

    else
        y


gap : Float
gap =
    1


boardGutter : Float
boardGutter =
    gap


pieceSize : Float
pieceSize =
    5


textHeight : Float
textHeight =
    2


pieceRenderPosition : ( Int, Int ) -> ( Float, Float )
pieceRenderPosition ( x, y ) =
    let
        pieceSizeWithGutter =
            pieceSize + boardGutter
    in
    ( toFloat x * pieceSizeWithGutter
    , toFloat y * pieceSizeWithGutter
    )


svgWidth : Float
svgWidth =
    pieceRenderPosition ( Board.numCols, Board.numRows )
        |> (\( w, _ ) -> w - boardGutter)


svgBoardHeight : Float
svgBoardHeight =
    pieceRenderPosition ( Board.numCols, Board.numRows )
        |> (\( _, h ) -> h - boardGutter)


svgHeight : Float
svgHeight =
    svgBoardHeight + gap + textHeight + gap + textHeight


view : Model -> Html Msg
view model =
    H.div [ HA.class "gameContainer" ]
        [ S.svg
            [ SA.viewBox
                ([ -gap
                 , -gap
                 , svgWidth + 2 * gap
                 , svgHeight + 2 * gap
                 ]
                    |> List.map String.fromFloat
                    |> String.join " "
                )
            , SA.id "gameView"
            ]
            [ if Dict.isEmpty model.removedPieces && Dict.isEmpty model.fallingPieces then
                HL.lazy5 viewBoard
                    0
                    model.board
                    model.removedPieces
                    model.fallingPieces
                    model.heldPiece

              else
                viewBoard model.time model.board model.removedPieces model.fallingPieces model.heldPiece
            , S.g
                [ SA.transform <|
                    "translate("
                        ++ String.fromFloat 0
                        ++ " "
                        ++ String.fromFloat (svgBoardHeight + gap)
                        ++ ")"
                ]
                [ HL.lazy2 viewScore model.score model.highScore
                ]
            ]
        , if model.isGameOver then
            HL.lazy2 viewGameOver model.score model.isNewHighScore

          else
            H.text ""
        ]


viewBoard : Float -> Board -> RemovedPieces -> FallingPieces -> Maybe HeldPiece -> Svg Msg
viewBoard time board removedPieces fallingPieces heldPiece =
    let
        viewRow y row =
            Array.toList row
                |> List.indexedMap
                    (\x piece ->
                        viewPiece
                            { now = time
                            , x = x
                            , y = y
                            , piece = piece
                            , state = boardPieceState ( x, y ) fallingPieces heldPiece
                            }
                    )
    in
    S.g []
        [ S.g [] <| viewRemovedPieces time board removedPieces
        , S.g []
            (Array.toList board
                |> List.indexedMap viewRow
                |> List.concat
            )
        , case heldPiece of
            Just hp ->
                viewPiece
                    { now = time
                    , x = Tuple.first hp.pos
                    , y = Tuple.second hp.pos
                    , piece = hp.piece
                    , state = PieceHeld hp
                    }

            Nothing ->
                S.text ""
        ]


boardPieceState :
    ( Int, Int )
    -> Dict ( Int, Int ) FallingPiecesData
    -> Maybe HeldPiece
    -> PieceState
boardPieceState ( x, y ) fallingPieces heldPiece =
    let
        isHidden =
            case heldPiece of
                Just hp ->
                    ( x, y ) == hp.pos

                Nothing ->
                    False
    in
    if isHidden then
        PieceHidden

    else
        case heldPiece of
            Just hp ->
                let
                    dir =
                        getDirection hp.startPoint hp.gamePos

                    dx =
                        Tuple.first hp.startPoint - Tuple.first hp.gamePos

                    dy =
                        Tuple.second hp.startPoint - Tuple.second hp.gamePos
                in
                if hp.startPoint == hp.gamePos then
                    PieceIdle

                else if dir == Up && hp.pos == ( x, y - 1 ) then
                    PieceSwitching ( 0, dy )

                else if dir == Down && hp.pos == ( x, y + 1 ) then
                    PieceSwitching ( 0, dy )

                else if dir == Left && hp.pos == ( x + 1, y ) then
                    PieceSwitching ( dx, 0 )

                else if dir == Right && hp.pos == ( x - 1, y ) then
                    PieceSwitching ( dx, 0 )

                else
                    PieceIdle

            Nothing ->
                case Dict.get ( x, y ) fallingPieces of
                    Just falling ->
                        PieceFalling falling

                    Nothing ->
                        PieceIdle


type Direction
    = Up
    | Down
    | Left
    | Right


getDirection : ( number, number ) -> ( number, number ) -> Direction
getDirection ( ax, ay ) ( bx, by ) =
    let
        dx =
            ax - bx

        dy =
            ay - by
    in
    if abs dx > abs dy then
        if dx > 0 then
            Left

        else
            Right

    else if dy > 0 then
        Down

    else
        Up


viewRemovedPieces : Float -> Board -> RemovedPieces -> List (Svg Msg)
viewRemovedPieces time board removedPieces =
    let
        viewRow y row =
            Array.toList row
                |> List.indexedMap
                    (\x _ ->
                        case Dict.get ( x, y ) removedPieces of
                            Just p ->
                                viewPiece
                                    { now = time
                                    , x = x
                                    , y = y
                                    , piece = p.piece
                                    , state = PieceRemoving { start = p.start }
                                    }

                            Nothing ->
                                H.text ""
                    )
    in
    Array.toList board
        |> List.indexedMap viewRow
        |> List.concat


type PieceState
    = PieceIdle
    | PieceHidden
    | PieceHeld HeldPiece
    | PieceRemoving { start : Float }
    | PieceFalling { start : Float, duration : Float, distance : Int }
    | PieceSwitching ( Float, Float )


viewPiece :
    { now : Float
    , x : Int
    , y : Int
    , piece : Piece
    , state : PieceState
    }
    -> Html Msg
viewPiece { now, x, y, piece, state } =
    let
        ( colorClass, symbol ) =
            case piece of
                Red ->
                    ( "-red", "★" )

                Orange ->
                    ( "-orange", "U" )

                Brown ->
                    ( "-brown", "I" )

                Yellow ->
                    ( "-yellow", "▲" )

                Greelow ->
                    ( "-greelow", "S" )

                Green ->
                    ( "-green", "●" )

                Cyan ->
                    ( "-cyan", "T" )

                Blue ->
                    ( "-blue", "◆" )

                Purple ->
                    ( "-purple", "■" )

                Pink ->
                    ( "-pink", "!" )

                White ->
                    ( "-white", "V" )

        ( xPos, yPos, otherAttrs ) =
            pieceRenderPosition ( x, y )
                |> (\( xp, yp ) ->
                        let
                            ( minx, miny ) =
                                pieceRenderPosition ( x - 1, y - 1 )

                            ( maxx, maxy ) =
                                pieceRenderPosition ( x + 1, y + 1 )
                        in
                        case state of
                            PieceIdle ->
                                ( xp, yp, [] )

                            PieceHidden ->
                                ( xp, yp, [ HA.style "opacity" "0" ] )

                            PieceHeld hp ->
                                let
                                    ( hX, hY ) =
                                        hp.gamePos

                                    ( sX, sY ) =
                                        hp.startPoint

                                    ( xOffset, yOffset ) =
                                        ( 37 * (hX - sX)
                                        , 55 * (hY - sY)
                                        )

                                    ( newXPos, newYPos ) =
                                        if abs xOffset > abs yOffset then
                                            -- snap piece to x axis
                                            ( xp + xOffset, yp )

                                        else
                                            -- snap piece to y axis
                                            ( xp, yp + yOffset )
                                in
                                ( clamp minx maxx newXPos
                                , clamp miny maxy newYPos
                                , [ -- SA.transform "translate(-2 -2)"
                                    SA.class "-held"
                                  ]
                                )

                            PieceRemoving f ->
                                let
                                    animationProgress =
                                        calcAnimationProgress now f.start removingAnimationDuration
                                            |> Ease.outQuad

                                    opacity =
                                        1 - animationProgress

                                    scale =
                                        1 - animationProgress

                                    blur =
                                        0.025 * animationProgress
                                in
                                ( xp
                                , yp
                                , [ SA.opacity <| String.fromFloat opacity
                                  , SA.transform <| "scale(" ++ String.fromFloat scale ++ ")"
                                  , SA.filter <| "blur(" ++ String.fromFloat blur ++ "rem)"
                                  , SA.pointerEvents "none"
                                  ]
                                )

                            PieceFalling f ->
                                let
                                    totalDistance =
                                        toFloat f.distance * (pieceSize + boardGutter)

                                    animationProgress =
                                        calcAnimationProgress now f.start f.duration
                                            |> fallingAnimationEasing f.distance

                                    yOffset =
                                        totalDistance * (1 - animationProgress)
                                in
                                ( xp, yp - yOffset, [] )

                            PieceSwitching ( dx, dy ) ->
                                ( xp + (37 * dx) |> clamp minx maxx
                                , yp + (55 * dy) |> clamp miny maxy
                                , []
                                )
                   )

        grabDecoder =
            JD.map (GrabbedPiece piece ( x, y )) mouseEventPositionDecoder
    in
    S.g
        [ SA.transform <|
            "translate("
                ++ String.fromFloat (xPos + (pieceSize / 2))
                ++ " "
                ++ String.fromFloat (yPos + (pieceSize / 2))
                ++ ")"
        ]
        [ S.g
            ([ SA.class <| "gamePiece " ++ colorClass
             , SE.on "mousedown" grabDecoder
             , SE.on "touchstart" grabDecoder
             ]
                ++ otherAttrs
            )
            [ S.circle
                [ SA.r <| String.fromFloat <| pieceSize / 2
                ]
                []
            , S.text_
                [ SA.textAnchor "middle"
                , SA.dy "0.7"
                , SA.fontSize "2"
                ]
                [ S.text symbol ]
            ]
        ]


viewGameOver : Int -> Bool -> Html Msg
viewGameOver score isNewHighScore =
    H.div [ HA.class "gameOverScreen" ]
        [ H.div [ HA.class "gameOverScreen_text" ]
            [ H.p [ HA.class "gameOverScreen_title" ] [ H.text "Game over" ]
            , H.div []
                [ H.p []
                    [ if isNewHighScore then
                        H.text <| "New high score! " ++ String.fromInt score

                      else
                        H.text <| "Score: " ++ String.fromInt score
                    ]
                , H.p []
                    [ H.button
                        [ HA.type_ "button"
                        , HE.onClick PlayAgainClicked
                        ]
                        [ H.text "Play again?" ]
                    ]
                ]
            , if score > Board.myHighScore then
                H.div
                    [ HA.class "gameOverScreen_congrats"
                    ]
                    [ H.p []
                        [ H.text "Congratulations!"
                        , H.br [] []
                        , H.text "You beat my high score."
                        , H.br [] []
                        , H.text "Thanks for playing!"
                        ]
                    , H.p []
                        [ externalLink
                            [ HA.href "https://github.com/megapctr/gems-game"
                            , HA.style "font-size" "1.2rem"
                            ]
                            [ H.text "View source code ↗" ]
                        ]
                    ]

              else
                H.text ""
            ]
        ]


viewScore : Int -> Int -> Html Msg
viewScore score highScore =
    let
        text attrs content =
            S.text_
                ((SA.fontSize <| String.fromFloat textHeight)
                    :: (SA.dy <| String.fromFloat (textHeight * 0.8))
                    :: attrs
                )
                [ S.text content ]
    in
    S.g []
        [ text []
            ("Score: " ++ String.fromInt score)
        , text
            [ SA.y <| String.fromFloat (gap + textHeight)
            ]
            ("High score: " ++ String.fromInt (max highScore score))
        ]


externalLink : List (Attribute msg) -> List (Html msg) -> Html msg
externalLink otherAttrs children =
    let
        attrs =
            HA.target "_blank"
                :: HA.rel "noopener noreferrer"
                :: otherAttrs
    in
    H.a attrs children
