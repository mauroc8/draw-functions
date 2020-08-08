module ErrorMessage exposing (view)

import Html
import Html.Attributes as Attr
import Parser



--- ERROR MESSAGE


type ErrorMessage
    = ExpectingValue Int
    | ExpectingClosingParenthesis Int
    | ExpectingOpeningParenthesis Int
    | ExpectingEnd Int
    | UnknownError


view : String -> List Parser.DeadEnd -> Html.Html a
view inputValue deadEnds =
    let
        errorMessage =
            fromDeadEnds deadEnds

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
                    , Html.span [ Attr.class functionFragmentClass ] [ Html.text "2x^2+x-5" ]
                    , Html.text "."
                    , Html.br [] []
                    , Html.text "If you're lost, please read the "
                    , Html.a
                        [ Attr.href "https://github.com/mauroc8/draw-functions"
                        , Attr.class "text-blue-500 hover:text-green-500"
                        ]
                        [ Html.text "syntax reference" ]
                    , Html.text "!"
                    ]

                else
                    [ viewFunctionWithError col
                    , Html.text "We couldn't understand some characters at the end of your function. "
                    , Html.text "Try deleting the last part!"
                    ]

            UnknownError ->
                [ Html.i [] [ Html.text "(visibly awkard) " ]
                , Html.text "Mmm... seems like we can't read your function, and we don't know exactly why. "
                , Html.text "Please, read the "
                , Html.a [ Attr.href "https://github.com/mauroc8/draw-functions" ]
                    [ Html.text "syntax reference" ]
                , Html.text " to learn how to use this app. "
                , Html.text "If you think this is a bug, please report an issue in the "
                , Html.a
                    [ Attr.href "https://github.com/mauroc8/draw-functions"
                    , Attr.class "text-blue-500 hover:text-green-500"
                    ]
                    [ Html.text "github repository" ]
                , Html.text "!"
                ]
        )


fromDeadEnds : List Parser.DeadEnd -> ErrorMessage
fromDeadEnds deadEnds =
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
                        fromDeadEnds deadEndsTail

                Parser.ExpectingEnd ->
                    ExpectingEnd col

                _ ->
                    fromDeadEnds deadEndsTail

        _ ->
            UnknownError
