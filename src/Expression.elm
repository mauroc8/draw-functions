module Expression exposing (Expression, parser, toStringFunction)

import Parser exposing ((|.), (|=), Parser)


parser : Parser Expression
parser =
    Parser.succeed identity
        |= Parser.oneOf
            [ expressionParser

            {- We don't want to have an error message when the input's empty.
               Maybe the user just wanted to draw one function, instead of two, so (s)he leaves the
               second input empty. Or maybe (s)he's just thinking what to write next...

               We parse empty strings successfully as the function "y = -99999999". That number is so
               big that we'll never actually draw that function in the graph.
            -}
            , Parser.succeed
                (Expression (Subtracted (Term (Multiplied (SimpleFactor (NumberLiteral 99999999))) [])) [])
            ]
        |. Parser.end


type Value
    = NumberLiteral Float
    | Variable
    | Parenthesized Expression
    | FunctionCall FunctionName Expression
    | ConstantPI
    | ConstantE


type FunctionName
    = Sin
    | Cos
    | Tan
    | Abs
    | Asin
    | Acos
    | Atan
    | Sqrt


type Factor
    = SimpleFactor Value
    | Power Value Value
      -- This is just syntax sugar for `*`.
      -- E.g. we parse `1/2x` as `1/(2*x)`,
      -- also works with `2sin(x)` or `(x + 2)(x - 2)`
      -- It has more precedence than normal multiplication (otherwise `1/2x` would be `(1/2)*x`)
    | MultipliedFactor Value Factor


type Term
    = Term TermUnit (List TermUnit)


type TermUnit
    = Multiplied Factor
    | Divided Factor


type Expression
    = Expression ExpressionUnit (List ExpressionUnit)


type ExpressionUnit
    = Added Term
    | Subtracted Term



--- PARSERS


valueParser : Parser Value
valueParser =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed NumberLiteral
                |= Parser.float
            , Parser.succeed Variable
                |. Parser.symbol "x"
            , Parser.succeed ConstantPI
                |. Parser.symbol "PI"
            , Parser.succeed ConstantE
                |. Parser.symbol "E"
            , Parser.succeed Parenthesized
                |. Parser.symbol "("
                |. Parser.spaces
                |= Parser.lazy (\_ -> expressionParser)
                |. Parser.spaces
                |. Parser.symbol ")"
            , Parser.succeed FunctionCall
                |= functionNameParser
                |. Parser.symbol "("
                |= Parser.lazy (\_ -> expressionParser)
                |. Parser.symbol ")"
            ]
        |. Parser.spaces


functionNameParser : Parser FunctionName
functionNameParser =
    Parser.oneOf
        [ Parser.succeed Sin
            |. Parser.keyword "sin"
        , Parser.succeed Sin
            |. Parser.keyword "sen"
        , Parser.succeed Cos
            |. Parser.keyword "cos"
        , Parser.succeed Tan
            |. Parser.keyword "tan"
        , Parser.succeed Abs
            |. Parser.keyword "abs"
        , Parser.succeed Asin
            |. Parser.keyword "asin"
        , Parser.succeed Asin
            |. Parser.keyword "asen"
        , Parser.succeed Acos
            |. Parser.keyword "acos"
        , Parser.succeed Atan
            |. Parser.keyword "atan"
        , Parser.succeed Sqrt
            |. Parser.keyword "sqrt"
        ]


factorParser : Parser Factor
factorParser =
    valueParser
        |> Parser.andThen
            (\value ->
                Parser.oneOf
                    [ Parser.succeed (Power value)
                        |. Parser.symbol "^"
                        |= valueParser
                    , Parser.backtrackable
                        (Parser.succeed (MultipliedFactor value)
                            |= Parser.lazy (\_ -> factorParser)
                        )
                    , Parser.succeed (SimpleFactor value)
                    ]
            )


termParser : Parser Term
termParser =
    Parser.succeed Term
        |= Parser.map Multiplied factorParser
        |= Parser.loop [] termParserHelp


termParserHelp : List TermUnit -> Parser (Parser.Step (List TermUnit) (List TermUnit))
termParserHelp parsedTermUnits =
    Parser.oneOf
        [ Parser.succeed (\value -> Parser.Loop (Multiplied value :: parsedTermUnits))
            |. Parser.symbol "*"
            |. Parser.spaces
            |= factorParser
        , Parser.succeed (\value -> Parser.Loop (Divided value :: parsedTermUnits))
            |. Parser.symbol "/"
            |. Parser.spaces
            |= factorParser
        , Parser.succeed (Parser.Done (List.reverse parsedTermUnits))
        ]


expressionParser : Parser Expression
expressionParser =
    Parser.succeed Expression
        |= Parser.oneOf
            [ Parser.succeed Subtracted
                |. Parser.symbol "-"
                |= termParser
            , Parser.succeed Added
                |. Parser.symbol "+"
                |= termParser
            , Parser.succeed Added
                |= termParser
            ]
        |= Parser.loop [] expressionParserHelp


expressionParserHelp : List ExpressionUnit -> Parser (Parser.Step (List ExpressionUnit) (List ExpressionUnit))
expressionParserHelp parsedExpressionUnits =
    Parser.oneOf
        [ Parser.succeed (\term -> Parser.Loop (Added term :: parsedExpressionUnits))
            |. Parser.symbol "+"
            |. Parser.spaces
            |= termParser
        , Parser.succeed (\term -> Parser.Loop (Subtracted term :: parsedExpressionUnits))
            |. Parser.symbol "-"
            |. Parser.spaces
            |= termParser
        , Parser.succeed (Parser.Done (List.reverse parsedExpressionUnits))
        ]



--- TO STRING


toStringFunction : Expression -> String
toStringFunction =
    expressionToString


expressionToString : Expression -> String
expressionToString (Expression firstUnit units) =
    List.foldl
        (\unit string -> string ++ expressionUnitToString unit)
        (case firstUnit of
            Added term ->
                termToString term

            Subtracted term ->
                "-" ++ termToString term
        )
        units


expressionUnitToString : ExpressionUnit -> String
expressionUnitToString unit =
    case unit of
        Added term ->
            " + " ++ termToString term

        Subtracted term ->
            " - " ++ termToString term


termToString : Term -> String
termToString (Term firstValue values) =
    List.foldl
        (\unit string -> string ++ termUnitToString unit)
        (case firstValue of
            Multiplied factor ->
                factorToString factor

            Divided factor ->
                "1/" ++ factorToString factor
        )
        values


termUnitToString : TermUnit -> String
termUnitToString termUnit =
    case termUnit of
        Multiplied factor ->
            " * " ++ factorToString factor

        Divided factor ->
            " / " ++ factorToString factor


factorToString : Factor -> String
factorToString factor =
    case factor of
        SimpleFactor value ->
            valueToString value

        Power value value2 ->
            "Math.pow(" ++ valueToString value ++ ", " ++ valueToString value2 ++ ")"

        MultipliedFactor value factor2 ->
            -- Writing `1/2x` should be interpreted as `1/(2*x)` that's why we use parenthesis
            "(" ++ valueToString value ++ "*" ++ factorToString factor2 ++ ")"


valueToString : Value -> String
valueToString value =
    case value of
        NumberLiteral float ->
            String.fromFloat float

        Variable ->
            "x"

        Parenthesized expr ->
            "(" ++ expressionToString expr ++ ")"

        FunctionCall fnName expr ->
            functionNameToString fnName ++ "(" ++ expressionToString expr ++ ")"

        ConstantPI ->
            "Math.PI"

        ConstantE ->
            "Math.E"


functionNameToString : FunctionName -> String
functionNameToString name =
    case name of
        Sin ->
            "Math.sin"

        Cos ->
            "Math.cos"

        Tan ->
            "Math.tan"

        Abs ->
            "Math.abs"

        Asin ->
            "Math.asin"

        Acos ->
            "Math.acos"

        Atan ->
            "Math.atan"

        Sqrt ->
            "Math.sqrt"
