port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import ErrorMessage
import Expression
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Evts
import Json.Decode
import Json.Encode
import Parser exposing ((|.), (|=), Parser)
import Point exposing (Point)
import Task


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--- MODEL


type alias Model =
    { firstFunction : FunctionModel
    , secondFunction : FunctionModel
    , graphWidth : Int
    , graphHeight : Int
    , offset : Point
    , scale : Point
    , mouseState : MouseState
    , isResizingGraph : Bool
    , windowHeight : Int
    , touchState : TouchState
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        ( firstInputValue, secondInputValue ) =
            case Json.Decode.decodeValue flagsDecoder flags of
                Ok result ->
                    ( result.firstInputValue, result.secondInputValue )

                Err _ ->
                    ( "sin(x)", "" )
    in
    ( { firstFunction =
            updateFunctionModel firstInputValue
                { inputValue = firstInputValue
                , parseError = Nothing
                , stringFunction = "-9999"
                , color = Blue
                }
      , secondFunction =
            updateFunctionModel secondInputValue
                { inputValue = secondInputValue
                , parseError = Nothing
                , stringFunction = "-9999"
                , color = Green
                }
      , graphWidth = 300
      , graphHeight = 350
      , offset = { x = 160, y = 160 }
      , scale = { x = 20, y = 20 }
      , mouseState = NotClicking
      , isResizingGraph = False
      , windowHeight = 100
      , touchState = NotTouching
      }
    , Task.perform GotBrowserViewport Browser.Dom.getViewport
    )


type alias Flags =
    { firstInputValue : String, secondInputValue : String }


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map2 Flags
        (Json.Decode.field "firstInputValue" Json.Decode.string)
        (Json.Decode.field "secondInputValue" Json.Decode.string)


type alias FunctionModel =
    { inputValue : String
    , parseError : Maybe ( List Parser.DeadEnd, ErrorViewState )
    , stringFunction : String
    , color : FunctionColor
    }


type FunctionColor
    = Blue
    | Green


type ErrorViewState
    = Opened
    | Closed


type MouseState
    = NotClicking
    | Clicking Point
    | Dragging Point Point


type TouchState
    = NotTouching
    | DraggingTouch Point Point
    | Scaling ( Point, Point ) ( Point, Point )



--- UPDATE


type Msg
    = NoOp
    | UpdateFirstInputValue String
    | UpdateSecondInputValue String
    | ResizedWindow Int Int
    | GotBrowserViewport Browser.Dom.Viewport
    | MouseDownOnGraph Point
    | MouseMovedOnGraph Point
    | MouseUp
    | WheelEvtOnGraph Float Point
    | ClickedOnHelpButton FunctionColor
    | StartedResizingGraph
    | MouseMovedWhileResizingGraph Point
    | TouchStartedOnGraph (List Point)
    | TouchMovedOnGraph (List Point)
    | TouchEnded
    | TouchMovedWhileResizingGraph (List Point)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateFirstInputValue string ->
            ( updateFirstInputValue string model
            , saveFirstInputValue string
            )

        UpdateSecondInputValue string ->
            ( updateSecondInputValue string model
            , saveSecondInputValue string
            )

        ResizedWindow width height ->
            ( updateWindowDimensions width height model
            , Cmd.none
            )

        GotBrowserViewport browserViewport ->
            ( updateWindowDimensions
                (floor browserViewport.viewport.width)
                (floor browserViewport.viewport.height)
                model
            , Cmd.none
            )

        MouseDownOnGraph position ->
            ( startClicking position model
            , Cmd.none
            )

        MouseMovedOnGraph position ->
            ( updateMouseState position model
            , Cmd.none
            )

        MouseUp ->
            ( stopClicking model
            , Cmd.none
            )

        WheelEvtOnGraph deltaY mouseCoords ->
            ( zoomGraphIntoPosition
                -- onwheel event has deltaY "inverted" in my PC
                -- The first argument here will be multiplied to the old scale
                (if deltaY > 0 then
                    0.9

                 else
                    1.1
                )
                mouseCoords
                model
            , Cmd.none
            )

        ClickedOnHelpButton color ->
            ( toggleHelpMessage color model
            , Cmd.none
            )

        StartedResizingGraph ->
            ( { model | isResizingGraph = True }
            , Cmd.none
            )

        MouseMovedWhileResizingGraph mouseCoords ->
            ( updateGraphHeight mouseCoords.y model
            , Cmd.none
            )

        TouchMovedWhileResizingGraph (touch :: touches) ->
            ( updateGraphHeight touch.y model
            , Cmd.none
            )

        TouchMovedWhileResizingGraph _ ->
            ( model, Cmd.none )

        TouchStartedOnGraph touches ->
            ( startTouching touches model, Cmd.none )

        TouchMovedOnGraph touches ->
            ( handleTouchMove touches model, Cmd.none )

        TouchEnded ->
            ( endTouching model, Cmd.none )


port saveFirstInputValue : String -> Cmd msg


port saveSecondInputValue : String -> Cmd msg


startTouching : List Point -> Model -> Model
startTouching touches model =
    case touches of
        touch1 :: touch2 :: touchesTail ->
            { model
                | touchState =
                    Scaling ( touch1, touch2 ) ( touch1, touch2 )
            }

        touch1 :: [] ->
            { model
                | touchState =
                    DraggingTouch touch1 touch1
            }

        _ ->
            model


handleTouchMove : List Point -> Model -> Model
handleTouchMove touches model =
    case ( model.touchState, touches ) of
        ( Scaling initial _, touch1 :: touch2 :: touchesTail ) ->
            { model
                | touchState =
                    Scaling initial ( touch1, touch2 )
            }

        ( DraggingTouch initial last, touch1 :: [] ) ->
            { model
                | touchState =
                    DraggingTouch initial touch1
            }

        _ ->
            model


endTouching : Model -> Model
endTouching model =
    case model.touchState of
        Scaling initial current ->
            zoomGraphIntoTouches
                initial
                current
                { model
                    | touchState = NotTouching
                }

        DraggingTouch initial current ->
            { model
                | touchState = NotTouching
                , offset = getDraggedOffset model
                , isResizingGraph = False
            }

        _ ->
            { model | isResizingGraph = False }


zoomGraphIntoTouches : ( Point, Point ) -> ( Point, Point ) -> Model -> Model
zoomGraphIntoTouches ( initial1, initial2 ) ( current1, current2 ) model =
    zoomGraphIntoPosition
        (clamp 0.2 2.5 <|
            Point.distance current1 current2
                / Point.distance initial1 initial2
        )
        (Point.average current1 current2)
        model


getDraggedOffset : Model -> Point
getDraggedOffset model =
    Point.add model.offset
        (case ( model.mouseState, model.touchState ) of
            ( Dragging initial curr, _ ) ->
                { x = curr.x - initial.x, y = initial.y - curr.y }

            ( _, DraggingTouch initial curr ) ->
                { x = curr.x - initial.x, y = initial.y - curr.y }

            _ ->
                { x = 0, y = 0 }
        )


getAdjustedOffsetAndScale : Model -> ( Point, Point )
getAdjustedOffsetAndScale model =
    case model.touchState of
        Scaling initial current ->
            let
                scaledModel =
                    zoomGraphIntoTouches
                        initial
                        current
                        model
            in
            ( scaledModel.offset, scaledModel.scale )

        _ ->
            ( getDraggedOffset model, model.scale )


updateFirstInputValue : String -> Model -> Model
updateFirstInputValue inputValue lastModel =
    { lastModel | firstFunction = updateFunctionModel inputValue lastModel.firstFunction }


updateSecondInputValue : String -> Model -> Model
updateSecondInputValue inputValue lastModel =
    { lastModel | secondFunction = updateFunctionModel inputValue lastModel.secondFunction }


updateFunctionModel : String -> FunctionModel -> FunctionModel
updateFunctionModel inputValue lastModel =
    let
        parseResult =
            Parser.run Expression.parser inputValue

        parseError =
            case parseResult of
                Err deadEnds ->
                    Just ( deadEnds, Closed )

                Ok _ ->
                    Nothing

        stringFunction =
            case parseResult of
                Err _ ->
                    lastModel.stringFunction

                Ok expr ->
                    Expression.toStringFunction expr
    in
    { lastModel
        | inputValue = inputValue
        , parseError = parseError
        , stringFunction = stringFunction
    }


updateWindowDimensions : Int -> Int -> Model -> Model
updateWindowDimensions width height model =
    { model
        | graphWidth = width
        , graphHeight = clamp 100 (height - 100) model.graphHeight
        , windowHeight = height
    }


updateGraphHeight : Int -> Model -> Model
updateGraphHeight height model =
    let
        newHeight =
            clamp 100 (model.windowHeight - 100) height
    in
    { model
        | graphHeight =
            newHeight
        , offset =
            { x = model.offset.x
            , y =
                -- Since the offset is from the bottom left corner,
                -- changing the graph height and keeping the offset as it is
                -- would make it look like the upper corner of the graph is moving up.
                -- This inversion makes the upper corner static while resizing the graph.
                model.offset.y - model.graphHeight + newHeight
            }
    }


zoomGraphIntoPosition : Float -> Point -> Model -> Model
zoomGraphIntoPosition scaleAdjustment mousePosition model =
    let
        newScale =
            { x =
                toFloat model.scale.x
                    |> (*) scaleAdjustment
                    |> round
                    |> clamp 2 400
            , y =
                toFloat model.scale.y
                    |> (*) scaleAdjustment
                    |> round
                    |> clamp 2 400
            }

        realScaleAdjustment =
            { x =
                toFloat newScale.x / toFloat model.scale.x
            , y =
                toFloat newScale.y / toFloat model.scale.y
            }
    in
    { model
        | scale =
            newScale

        {- This heavy math is just ensuring that

               viewportToWorld(mousePosition, oldScale, oldOffset)
                   == viewportToWorld(mousePosition, newScale, newOffset)

           i.e., we want to zoom into the current mouse position

           The definition of viewportToWorld is inside graph.js

        -}
        , offset =
            { x =
                (1 - realScaleAdjustment.x)
                    * toFloat mousePosition.x
                    + realScaleAdjustment.x
                    * toFloat model.offset.x
                    |> round
            , y =
                (1 - realScaleAdjustment.y)
                    * toFloat model.graphHeight
                    + (realScaleAdjustment.y - 1)
                    * toFloat mousePosition.y
                    + realScaleAdjustment.y
                    * toFloat model.offset.y
                    |> round
            }
    }


startClicking : Point -> Model -> Model
startClicking position model =
    { model
        | mouseState = Clicking position
    }


updateMouseState : Point -> Model -> Model
updateMouseState currentPosition model =
    case model.mouseState of
        NotClicking ->
            model

        Clicking initial ->
            { model
                | mouseState =
                    Dragging initial currentPosition
            }

        Dragging initial _ ->
            { model
                | mouseState =
                    Dragging initial currentPosition
            }


stopClicking : Model -> Model
stopClicking model =
    { model
        | mouseState = NotClicking
        , offset = getDraggedOffset model
        , isResizingGraph = False
    }


toggleHelpMessage : FunctionColor -> Model -> Model
toggleHelpMessage color model =
    { model
        | firstFunction =
            toggleFunctionHelpMessage color model.firstFunction
        , secondFunction =
            toggleFunctionHelpMessage color model.secondFunction
    }


toggleFunctionHelpMessage : FunctionColor -> FunctionModel -> FunctionModel
toggleFunctionHelpMessage color functionModel =
    if functionModel.color == color then
        { functionModel
            | parseError =
                case functionModel.parseError of
                    Just ( deadEnds, Opened ) ->
                        Just ( deadEnds, Closed )

                    Just ( deadEnds, Closed ) ->
                        Just ( deadEnds, Opened )

                    Nothing ->
                        Nothing
        }

    else
        functionModel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize ResizedWindow
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        , if model.isResizingGraph then
            Sub.batch
                [ Browser.Events.onMouseMove
                    (Json.Decode.map MouseMovedWhileResizingGraph mouseEventDecoder)
                , onTouchMove
                    (\value ->
                        Json.Decode.decodeValue
                            (Json.Decode.map TouchMovedWhileResizingGraph touchEventDecoder)
                            value
                            |> Result.withDefault NoOp
                    )
                ]

          else
            Sub.none
        , onTouchEnd (always TouchEnded)
        ]


port onTouchEnd : (Json.Decode.Value -> msg) -> Sub msg


port onTouchMove : (Json.Decode.Value -> msg) -> Sub msg



--- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "font-sans text-gray-900 w-full"
        ]
        [ Html.h1
            [ Attr.class "font-bold mx-2 mt-2 text-gray-600 absolute hover:text-gray-800"
            , Attr.class "transition duration-300 text-sm"
            ]
            [ Html.text "DRAW FUNCTIONS" ]
        , graphElement model
        , Html.div
            [ Attr.class "block w-full bg-gray-200 cursor-row-resize h-1"
            , Evts.preventDefaultOn "mousedown"
                (Json.Decode.succeed StartedResizingGraph
                    |> Json.Decode.map alwaysPreventDefault
                )
            , Evts.preventDefaultOn "touchstart"
                (Json.Decode.succeed StartedResizingGraph
                    |> Json.Decode.map alwaysPreventDefault
                )
            ]
            []
        , Html.div
            [ Attr.class "md:flex" ]
            [ functionInput
                { functionModel = model.firstFunction
                , onInputMsg = UpdateFirstInputValue
                }
            , functionInput
                { functionModel = model.secondFunction
                , onInputMsg = UpdateSecondInputValue
                }
            ]
        , Html.div
            [ Attr.class "mx-2 mt-6 text-sm text-gray-600"
            ]
            [ Html.a
                [ Attr.href "https://github.com/mauroc8/draw-functions"
                , Attr.class "text-blue-400 hover:text-green-400"
                ]
                [ Html.text "View source & syntax reference"
                ]
            , Html.br [] []
            , Html.text "Mauro Cano © 2020"
            ]
        ]


graphElement : Model -> Html Msg
graphElement model =
    let
        ( draggedOffset, adjustedScale ) =
            getAdjustedOffsetAndScale model

        cursorClass =
            case model.mouseState of
                Dragging _ _ ->
                    "cursor-move"

                Clicking _ ->
                    "cursor-move"

                _ ->
                    "cursor-auto"
    in
    Html.node "graph-element"
        [ Attr.attribute "first-function" model.firstFunction.stringFunction
        , Attr.attribute "second-function" model.secondFunction.stringFunction
        , Attr.attribute "width" (String.fromInt model.graphWidth)
        , Attr.attribute "height" (String.fromInt model.graphHeight)
        , Attr.attribute "offset-x" (String.fromInt draggedOffset.x)
        , Attr.attribute "offset-y" (String.fromInt draggedOffset.y)
        , Attr.attribute "scale-x" (String.fromInt adjustedScale.x)
        , Attr.attribute "scale-y" (String.fromInt adjustedScale.y)
        , Evts.on "mousedown" (Json.Decode.map MouseDownOnGraph mouseEventDecoder)
        , Evts.on "mousemove" (Json.Decode.map MouseMovedOnGraph mouseEventDecoder)
        , Attr.class cursorClass
        , Evts.preventDefaultOn "wheel"
            (Json.Decode.map2 WheelEvtOnGraph wheelEventDecoder mouseEventDecoder
                |> Json.Decode.map alwaysPreventDefault
            )
        , Evts.preventDefaultOn "touchstart"
            (Json.Decode.map TouchStartedOnGraph touchEventDecoder
                |> Json.Decode.map alwaysPreventDefault
            )
        , Evts.preventDefaultOn "touchmove"
            (Json.Decode.map TouchMovedOnGraph touchEventDecoder
                |> Json.Decode.map alwaysPreventDefault
            )
        ]
        []



--- EVENT DECODERS
{- This multi-touch event decoder was written using
   https://github.com/mpizenberg/elm-touch-events/blob/master/src/MultiTouch.elm
   as a reference.
-}


touchEventDecoder : Json.Decode.Decoder (List Point)
touchEventDecoder =
    Json.Decode.field "targetTouches"
        (Json.Decode.field "length" Json.Decode.int
            |> Json.Decode.andThen
                (\length ->
                    List.range 0 (length - 1)
                        |> List.map decodeTouchAt
                        |> List.foldr (Json.Decode.map2 (::)) (Json.Decode.succeed [])
                )
        )


decodeTouchAt : Int -> Json.Decode.Decoder Point
decodeTouchAt x =
    Json.Decode.field (String.fromInt x) <|
        Json.Decode.map2 Point
            (Json.Decode.field "pageX" Json.Decode.float
                |> Json.Decode.map round
            )
            (Json.Decode.field "pageY" Json.Decode.float
                |> Json.Decode.map round
            )


wheelEventDecoder : Json.Decode.Decoder Float
wheelEventDecoder =
    Json.Decode.field "deltaY" Json.Decode.float


mouseEventDecoder : Json.Decode.Decoder Point
mouseEventDecoder =
    Json.Decode.map2 Point
        (Json.Decode.field "pageX" Json.Decode.float
            |> Json.Decode.map round
        )
        (Json.Decode.field "pageY" Json.Decode.float
            |> Json.Decode.map round
        )


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


functionInput :
    { functionModel : FunctionModel
    , onInputMsg : String -> Msg
    }
    -> Html Msg
functionInput { functionModel, onInputMsg } =
    let
        {- I write it like this because when we trim our unused Css classes,
           we need to have all the class names we're using written explicitly.
        -}
        color =
            functionModel.color

        textColor600 =
            case color of
                Blue ->
                    "text-blue-600"

                Green ->
                    "text-green-600"

        bgColor100 =
            case color of
                Blue ->
                    "bg-blue-100"

                Green ->
                    "bg-green-100"

        focusBorderColor500 =
            case color of
                Blue ->
                    "focus:border-blue-500"

                Green ->
                    "focus:border-green-500"
    in
    Html.div
        [ Attr.class "p-2 md:w-2/4"
        ]
        [ Html.span [ Attr.class ("font-mono " ++ textColor600) ] [ Html.text "y = " ]
        , Html.input
            [ Attr.class ("font-mono " ++ textColor600)
            , Attr.value functionModel.inputValue
            , Evts.onInput onInputMsg
            , Attr.class
                "rounded border transition duration-300 focus:bg-transparent px-1 focus:outline-none"
            , case functionModel.parseError of
                Just _ ->
                    Attr.class "border-red-400 bg-red-100"

                Nothing ->
                    Attr.class
                        ("border-gray-200 focus:border-opacity-50 "
                            ++ bgColor100
                            ++ " "
                            ++ focusBorderColor500
                        )
            , Attr.autofocus True
            , Attr.attribute "autocapitalize" "off"
            , Attr.spellcheck False
            ]
            []
        , case functionModel.parseError of
            Just ( deadEnds, openStatus ) ->
                Html.button
                    [ Attr.class "px-1 rounded mx-1 focus:outline-none border border-red-100 focus:border-red-500"
                    , Attr.class
                        (case openStatus of
                            Opened ->
                                "bg-red-400 text-white"

                            Closed ->
                                "bg-red-100"
                        )
                    , Evts.onClick (ClickedOnHelpButton color)
                    ]
                    [ Html.text "?" ]

            Nothing ->
                Html.text ""
        , case functionModel.parseError of
            Just ( deadEnds, Opened ) ->
                Html.div
                    [ Attr.class "text-xs bg-red-100 rounded-md p-2 border border-red-200 mt-2" ]
                    [ ErrorMessage.view functionModel.inputValue deadEnds ]

            _ ->
                Html.text ""
        ]
