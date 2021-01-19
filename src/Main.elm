port module Main exposing (..)


import Browser
import Browser.Events as Browser
import Html
import Html.Attributes exposing (style)

import Color
import Dict exposing (Dict)

import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Settings.Line as Canvas
import Canvas.Settings.Text as Text
import Canvas.Texture as Canvas

import CombinedTransform as Transform


apply : Transform.Matrix -> Canvas.Setting
apply =
    Canvas.transform
        << List.singleton
        << Canvas.applyMatrix


type TextureAlias = TextureAlias String


type alias Textures = Dict String Canvas.Texture

type alias Time = Float


type alias RenderState =
    { time : Time
    , textures : Textures
    , matrix : Transform.Matrix
    , knobValue : Maybe ( KnobId, Value )
    }


type alias Renderer a
    =  RenderState
    -> a
    -> List Canvas.Renderable


type Goose = Goose


type Highlander = ConnorMacLeod


stringRenderer : Renderer String
stringRenderer { matrix } str =
    [ Canvas.text
        [ Text.font
            { family = "sans-serif"
            , size = 48
            }
        , apply matrix
        ]
        ( 0, 0 )
        str
    ]


intRenderer : Renderer Int
intRenderer state n =
    stringRenderer
        state
        <| case n of
            0 -> "0️⃣"
            1 -> "1️⃣"
            2 -> "2️⃣"
            3 -> "3️⃣"
            4 -> "4️⃣"
            5 -> "5️⃣"
            6 -> "6️⃣"
            7 -> "7️⃣"
            8 -> "8️⃣"
            9 -> "9️⃣"
            _ -> "🤦‍♂️"


highlanderRenderer : Renderer Highlander
highlanderRenderer state _ =
    imageRenderer state <| TextureAlias "unnamed"


gooseRenderer : Renderer Goose
gooseRenderer state _ =
    stringRenderer state "鵞"


unitRenderer : Renderer ()
unitRenderer state _ =
    stringRenderer state "🤦‍♂️"


{- type Maybe a
    = Just a
    | Nothing -}


maybeRenderer : Renderer a -> Renderer (Maybe a)
maybeRenderer itemRenderer state maybeValue =
    case maybeValue of
        Just value ->
            itemRenderer state value
        Nothing -> stringRenderer state "◌"


type Either a b
    = Left a
    | Right b


eitherRenderer
    :  Renderer a
    -> Renderer b
    -> Renderer (Either a b)
eitherRenderer aRenderer bRenderer state eitherValue =
    case eitherValue of
        Left lValue ->
            aRenderer state lValue
        Right rValue ->
            bRenderer state rValue


eitherGenericRenderer : Renderer (Either a b)
eitherGenericRenderer state eitherValue =
    case eitherValue of
        Left _ ->
            stringRenderer state "◌"
        Right _ ->
            stringRenderer state "◌"


type Tuple a b = Tuple a b


tupleRenderer
    :  Renderer a
    -> Renderer b
    -> Renderer (Tuple a b)
tupleRenderer aRenderer bRenderer state (Tuple aValue bValue) =
       aRenderer state aValue
    ++ bRenderer state bValue


itemWidth = 65

itemHeight = 65


listRenderer : Renderer a -> Renderer (List a)
listRenderer itemRenderer state items =
    let
        distance = List.length items * (itemWidth + 15)
        renderItems locM innerItems =
            case innerItems of
                [] -> []
                first::rest ->
                       itemRenderer { state | matrix = locM } first
                    {-
                    ++ stringRenderer
                            textures
                            ( locM |> Transform.translate itemWidth 10 )
                            ","
                    -}
                    ++ renderItems
                        ( locM |> Transform.translate (itemWidth  + 15) 0 )
                        rest
    in
        renderItems
            ( state.matrix |> Transform.translate 10 45 )
            items


gridRenderer : Renderer a -> Renderer (List (List a))
gridRenderer itemRenderer state items =
    items
        |> List.indexedMap
            (\rowIdx row ->
                listRenderer
                    itemRenderer
                    { state
                    | matrix =
                        state.matrix
                        |> Transform.rotate (toFloat rowIdx * 2 * pi / toFloat (List.length items))
                        |> Transform.translate 0 (toFloat rowIdx * itemHeight)
                    }
                    row
            )
        |> List.concat


type Tree a
    = Branch (Tree a) a (Tree a)
    | Leaf a


treeRenderer : Renderer a -> Renderer (Tree a)
treeRenderer itemRenderer state tree =
    case tree of
        Leaf value ->
            itemRenderer state value
        Branch left value right ->
            treeRenderer
                itemRenderer
                { state
                | matrix =
                    state.matrix
                        |> Transform.translate -itemWidth (itemHeight + 10)
                }
                left
            ++ itemRenderer state value
            ++ treeRenderer
                itemRenderer
                { state
                | matrix =
                    state.matrix
                        |> Transform.translate itemWidth (itemHeight + 10)
                }
                right


spiralRenderer : Renderer a -> Renderer (List a)
spiralRenderer itemRenderer state items =
    let
        maxRadius = 150
        kv =
            case state.knobValue of
                Just ( _, value ) ->
                    toFloat value / 127
                Nothing -> 0
        radius idx =
            30 + (idx / toFloat (List.length items)) * maxRadius
        angleCoef = 2 * pi / toFloat (List.length items) * 4
        matrixFor idx angle =
            state.matrix
                |> Transform.translate
                    ((radius idx) * cos (idx * angleCoef))
                    ((radius idx) * sin (idx * angleCoef))
                |> Transform.translate 300 70
                -- |> Transform.rotate (degrees <| kv * idx * sin (state.time / 500))
                |> Transform.rotate (degrees <| idx * sin (state.time / 500))
        renderItems idx angle innerItems =
            case innerItems of
                [] -> []
                first::rest ->
                       itemRenderer
                            { state
                            | matrix =
                                matrixFor idx angle
                            }
                            first
                    ++ renderItems
                        (idx + 1)
                        (angle + 5)
                        rest
    in
        renderItems 0 0 items


type Polygon = Polygon Int


polygonRenderer : Renderer Polygon
polygonRenderer state (Polygon n) =
    let
        radius = 40
        angleCoef = 2 * pi / toFloat n
        pointFor idx =
            ( radius * cos (idx * angleCoef)
            , radius * sin (idx * angleCoef)
            )
    in
        [ Canvas.shapes
            [ Canvas.fill Color.black
            , Canvas.stroke Color.black
            , Canvas.lineWidth 5
            , apply state.matrix
            ]
            [ Canvas.path
                ( pointFor 0 )
                <| List.map (Canvas.lineTo << pointFor << toFloat)
                <| List.range 1 (n + 1)
            ]
        ]


texturesToLoad =
    [ ( "./assets/goose_drawn.svg", "goose_drawn" )
    , ( "./assets/goose.png", "goose" )
    , ( "./assets/goose.svg", "goose_logo" )
    , ( "./assets/unnamed.jpg", "unnamed" )
    ]


imageRenderer : Renderer TextureAlias
imageRenderer state (TextureAlias textureAlias) =
    case state.textures |> Dict.get textureAlias of
        Just texture ->
            [ Canvas.texture
                [ apply
                    (state.matrix
                        --|> Transform.rotate (2 * pi * (sin <| state.time / 2000))
                        --|> Transform.scale 0.25 0.25
                    )
                ]
                ( -20, 0 )
                texture

            ]
        Nothing -> []


type Msg
    = NoOp
    | GotTexture TextureAlias Canvas.Texture
    | Tick Float
    | MidiKnob ( KnobId, Value )


drawCanvas : RenderState -> Html.Html Msg
drawCanvas state =
    Canvas.toHtmlWith
        { height = 700
        , width = 700
        , textures =

            texturesToLoad
                |> List.map (Tuple.mapSecond TextureAlias)
                |> List.map
                    (\(path, textureAlias) ->
                        Canvas.loadFromImageUrl
                            path
                            (Maybe.map
                                (GotTexture textureAlias)
                                >> Maybe.withDefault NoOp
                            )
                    )

        }
        [ style "display" "block" ]

        <| Canvas.clear (0, 0) 700 700

        ::

            (let
                items = List.repeat 100 ()
                itemRenderer = unitRenderer
                matrix = Transform.init
            in
                spiralRenderer
                    itemRenderer
                    { state | matrix = matrix }
                    items
            ++ spiralRenderer
                itemRenderer
                ({ state | matrix =
                    matrix |> Transform.translate 30 60 })
                items
            ++ spiralRenderer
                itemRenderer
                    ({ state | matrix =
                        matrix
                            |> Transform.translate 200 100
                            |> Transform.rotate (degrees 30) }
                    )
                    items)

            {- ( gridRenderer
                imageRenderer
                { state
                | matrix = Transform.init
                }
                <| List.repeat 10 (List.repeat 10 <| TextureAlias "goose_drawn")

            ) -}

            {- ( treeRenderer
                gooseRenderer
                { state
                | matrix = Transform.init
                    |> Transform.translate 160 160
                }
                <| Branch
                    (Branch (Leaf Goose) Goose (Leaf Goose))
                    Goose
                    (Leaf Goose)
            ) -}

            {- ( spiralRenderer
                imageRenderer
                { state
                | matrix = Transform.init
                    |> Transform.rotate (degrees 20)
                    |> Transform.translate 500 550
                }
                (List.repeat 100 <| TextureAlias "goose_drawn")
            ) -}


update : Msg -> RenderState -> ( RenderState, Cmd Msg )
update msg state =
    ( case msg of
        NoOp -> state
        GotTexture (TextureAlias textureAlias) texture ->
            { state
            | textures
                = state.textures
                    |> Dict.insert textureAlias texture
            }
        Tick dt ->
            { state
            | time = state.time + dt
            }

        MidiKnob ( knobId, value ) ->
            { state
            | knobValue = Just ( knobId, value )
            }

    , Cmd.none
    )



init : () -> ( RenderState, Cmd Msg )
init _ =
    (
        { time = 0
        , textures = Dict.empty
        , matrix = Transform.init
        , knobValue = Nothing
        }
    , Cmd.none
    )


main : Program () RenderState Msg
main =
    Browser.element
        { init = init
        , view = drawCanvas
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.onAnimationFrameDelta Tick
                    , onMidiKnob MidiKnob
                    ]

        }

type alias KnobId = Int

type alias Value = Int


port onMidiKnob : ((KnobId, Value) -> msg) -> Sub msg
