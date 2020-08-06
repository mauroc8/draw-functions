port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Expression
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Evts
import Json.Decode
import Json.Encode
import Parser exposing ((|.), (|=), Parser)
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
    , offset : IntVector2
    , scale : IntVector2
    , mouseState : MouseState
    , isResizingGraph : Bool
    , windowHeight : Int
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
                }
      , secondFunction =
            updateFunctionModel secondInputValue
                { inputValue = secondInputValue
                , parseError = Nothing
                , stringFunction = "-9999"
                }
      , graphWidth = 300
      , graphHeight = 350
      , offset = { x = 160, y = 160 }
      , scale = { x = 20, y = 20 }
      , mouseState = NotClicking
      , isResizingGraph = False
      , windowHeight = 100
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
    }


type ErrorViewState
    = Opened
    | Closed


type MouseState
    = NotClicking
    | Clicking IntVector2
    | Dragging IntVector2 IntVector2


type alias IntVector2 =
    { x : Int, y : Int }


addIntVector2 : IntVector2 -> IntVector2 -> IntVector2
addIntVector2 a b =
    { x = a.x + b.x, y = a.y + b.y }



--- UPDATE


type Msg
    = NoOp
    | UpdateFirstInputValue String
    | UpdateSecondInputValue String
    | ResizedWindow Int Int
    | GotBrowserViewport Browser.Dom.Viewport
    | MouseDownOnGraph IntVector2
    | MouseMovedOnGraph IntVector2
    | MouseUp
    | WheelEvtOnGraph Float IntVector2
    | ClickedOnHelpButton FunctionColor
    | StartedResizingGraph
    | MouseMovedWhileResizingGraph IntVector2


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
            ( updateGraphScale deltaY mouseCoords model
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


port saveFirstInputValue : String -> Cmd msg


port saveSecondInputValue : String -> Cmd msg


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
    { model | graphHeight = clamp 100 (model.windowHeight - 100) height }


updateGraphScale : Float -> IntVector2 -> Model -> Model
updateGraphScale deltaY mousePosition model =
    let
        scaleAdjustment =
            if deltaY > 0 then
                0.9

            else
                1.1

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


startClicking : IntVector2 -> Model -> Model
startClicking position model =
    { model
        | mouseState = Clicking position
    }


updateMouseState : IntVector2 -> Model -> Model
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
    case color of
        Blue ->
            { model | firstFunction = toggleFunctionHelpMessage model.firstFunction }

        Green ->
            { model | secondFunction = toggleFunctionHelpMessage model.secondFunction }


toggleFunctionHelpMessage : FunctionModel -> FunctionModel
toggleFunctionHelpMessage functionModel =
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
                ]

          else
            Sub.none
        ]



--- VIEW


type FunctionColor
    = Blue
    | Green


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class "font-sans text-gray-900"
        ]
        [ Html.h1
            [ Attr.class "font-bold mx-2 mt-2 text-gray-600 absolute hover:text-gray-800 transition duration-300 text-sm" ]
            [ Html.text "DRAW FUNCTIONS" ]
        , graphElement model
        , Html.div
            [ Attr.class "block w-full bg-gray-200 cursor-row-resize h-1"
            , Evts.preventDefaultOn "mousedown"
                (Json.Decode.succeed StartedResizingGraph
                    |> Json.Decode.map alwaysPreventDefault
                )
            ]
            []
        , Html.div
            [ Attr.class "md:flex" ]
            [ functionInput
                { functionModel = model.firstFunction
                , color = Blue
                , onInputMsg = UpdateFirstInputValue
                }
            , functionInput
                { functionModel = model.secondFunction
                , color = Green
                , onInputMsg = UpdateSecondInputValue
                }
            ]
        ]


getDraggedOffset : Model -> IntVector2
getDraggedOffset model =
    addIntVector2 model.offset
        (case model.mouseState of
            Dragging initial curr ->
                { x = curr.x - initial.x, y = initial.y - curr.y }

            _ ->
                { x = 0, y = 0 }
        )


graphElement : Model -> Html Msg
graphElement model =
    let
        draggedOffset =
            getDraggedOffset model

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
        , Attr.attribute "scale-x" (String.fromInt model.scale.x)
        , Attr.attribute "scale-y" (String.fromInt model.scale.y)
        , Evts.on "mousedown" (Json.Decode.map MouseDownOnGraph mouseEventDecoder)
        , Evts.on "mousemove" (Json.Decode.map MouseMovedOnGraph mouseEventDecoder)
        , Attr.class cursorClass
        , Evts.preventDefaultOn "wheel"
            (Json.Decode.map2 WheelEvtOnGraph wheelEventDecoder mouseEventDecoder
                |> Json.Decode.map alwaysPreventDefault
            )
        ]
        []


wheelEventDecoder : Json.Decode.Decoder Float
wheelEventDecoder =
    Json.Decode.field "deltaY" Json.Decode.float


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


functionInput :
    { functionModel : FunctionModel
    , color : FunctionColor
    , onInputMsg : String -> Msg
    }
    -> Html Msg
functionInput { functionModel, color, onInputMsg } =
    let
        {- I write it like this because when we trim our unused Css classes,
           we need to have all the class names we're using written explicitly.
        -}
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
                    [ viewErrorMessage functionModel.inputValue deadEnds ]

            _ ->
                Html.text ""
        ]


mouseEventDecoder : Json.Decode.Decoder IntVector2
mouseEventDecoder =
    Json.Decode.map2 IntVector2
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)


type ErrorMessage
    = ExpectingValue Int
    | ExpectingClosingParenthesis Int
    | ExpectingOpeningParenthesis Int
    | ExpectingEnd Int
    | UnknownError


viewErrorMessage : String -> List Parser.DeadEnd -> Html a
viewErrorMessage inputValue deadEnds =
    let
        errorMessage =
            deadEndsToErrorMessage deadEnds

        functionFragmentClass =
            "font-mono bg-gray-200 px-1 rounded text-gray-700"

        viewFunctionWithError col =
            Html.pre []
                [ Html.text inputValue
                , Html.text "\n"
                , Html.text (String.repeat (col - 1) " " ++ "↑")
                ]
    in
    Html.div []
        (case errorMessage of
            ExpectingValue col ->
                [ viewFunctionWithError col
                , Html.text "We were expecting some value, like "
                , Html.span [ Attr.class functionFragmentClass ] [ Html.text "5" ]
                , Html.text ", "
                , Html.span [ Attr.class functionFragmentClass ] [ Html.text "2x" ]
                , Html.text " or "
                , Html.span [ Attr.class functionFragmentClass ] [ Html.text "sin(x)" ]
                , Html.text "..."
                ]

            ExpectingClosingParenthesis col ->
                [ viewFunctionWithError col
                , Html.text "We were expecting a closing parenthesis!"
                ]

            ExpectingOpeningParenthesis col ->
                [ viewFunctionWithError col
                , Html.text "We were expecting an opening parenthesis! Remember parenthesis are always "
                , Html.text " necessary when writing functions, for example: "
                , Html.span [ Attr.class functionFragmentClass ] [ Html.text "sin(x)" ]
                , Html.text "."
                ]

            ExpectingEnd col ->
                if col == 1 then
                    [ viewFunctionWithError col
                    , Html.text "We couldn't understand your function. Try something like "
                    , Html.span [ Attr.class functionFragmentClass ] [ Html.text "sin(x)" ]
                    , Html.text " or "
                    , Html.span [ Attr.class functionFragmentClass ] [ Html.text "2x^2 + x - 5" ]
                    , Html.text "... If you're lost, please read the tutorial!"
                    ]

                else
                    [ viewFunctionWithError col
                    , Html.text "We couldn't understand some characters at the end of your function. "
                    , Html.text "Try deleting the last part!"
                    ]

            UnknownError ->
                [ Html.i [] [ Html.text "(visibly awkard) " ]
                , Html.text "Mmm... seems like we can't read your function, and we don't know exactly why. "
                , Html.text "Please, read the tutorial to learn how to use this app. "
                , Html.text "If you think this is a bug, please report an issue in the github repository!"
                ]
        )


deadEndsToErrorMessage : List Parser.DeadEnd -> ErrorMessage
deadEndsToErrorMessage deadEnds =
    let
        maximumCol =
            List.foldl (.col >> max) 0 deadEnds

        filteredDeadEnds =
            List.filter (.col >> (==) maximumCol) deadEnds
    in
    case deadEnds of
        { col, problem } :: deadEndsTail ->
            case problem of
                Parser.ExpectingFloat ->
                    ExpectingValue col

                Parser.ExpectingSymbol sym ->
                    if sym == ")" then
                        ExpectingClosingParenthesis col

                    else if sym == "(" then
                        ExpectingOpeningParenthesis col

                    else if sym == "x" then
                        ExpectingValue col

                    else
                        deadEndsToErrorMessage deadEndsTail

                Parser.ExpectingEnd ->
                    ExpectingEnd col

                _ ->
                    deadEndsToErrorMessage deadEndsTail

        _ ->
            UnknownError
