module Main exposing (main)

import Browser
import Char
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String


type Operator
    = Add
    | Sub
    | Mult
    | Div
    | None


type alias Model =
    { operand1 : Float
    , operand2 : Float
    , operator : Operator
    , result : String
    , upperDisplay : String
    , lowerDisplay : String
    }


init : Model
init =
    { operand1 = 0
    , operand2 = 0
    , operator = None
    , result = ""
    , upperDisplay = ""
    , lowerDisplay = "0"
    }


type Msg
    = AppendToOperand Float
    | Clear
    | ClearEntry
    | UpdateOperator Operator
    | Calculate


update : Msg -> Model -> Model
update msg model =
    case msg of
        AppendToOperand num ->
            if model.operator == None then
                { model | operand1 = model.operand1 * 10 + num, lowerDisplay = String.fromFloat (model.operand1 * 10 + num) }

            else
                { model | operand2 = model.operand2 * 10 + num, lowerDisplay = String.fromFloat (model.operand2 * 10 + num) }

        Clear ->
            init

        ClearEntry ->
            if model.result /= "" then
                init

            else if model.operator /= None then
                { model | operand2 = 0, operator = None, upperDisplay = String.fromFloat model.operand1, lowerDisplay = "0" }

            else
                { model | operand1 = 0, lowerDisplay = "0" }

        UpdateOperator op ->
            let
                operatorSymbol =
                    case op of
                        Add ->
                            "+"

                        Sub ->
                            "-"

                        Mult ->
                            "*"

                        Div ->
                            "/"

                        None ->
                            ""
            in
            { model | operator = op, upperDisplay = String.fromFloat model.operand1 ++ " " ++ operatorSymbol }

        Calculate ->
            let
                res =
                    case model.operator of
                        Add ->
                            String.fromFloat (model.operand1 + model.operand2)

                        Sub ->
                            String.fromFloat (model.operand1 - model.operand2)

                        Mult ->
                            String.fromFloat (model.operand1 * model.operand2)

                        Div ->
                            if model.operand2 == 0 then
                                "Error: Division by zero"

                            else
                                String.fromFloat (model.operand1 / model.operand2)

                        None ->
                            "Error: Unknown operation"
            in
            { model | result = res, lowerDisplay = res, upperDisplay = model.upperDisplay ++ " " ++ String.fromFloat model.operand2 ++ " = " }


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ div [ class "display-container" ]
            [ div [ class "upperDisplay" ] [ text model.upperDisplay ]
            , div [ class "lowerDisplay" ] [ text model.lowerDisplay ]
            ]
        , div [ class "button-container" ]
            [ button [ class "button ce-button", onClick ClearEntry ] [ text "CE" ]
            , button [ class "button c-button", onClick Clear ] [ text "C" ]
            ]
        , div [ class "button-container" ]
            (List.map (\operand -> button [ class "button operand-button", onClick <| AppendToOperand (toFloat operand) ] [ text (String.fromInt operand) ]) (List.range 7 9)
                ++ [ button [ class "button operation-button", onClick <| UpdateOperator Div ] [ text (String.fromChar (Char.fromCode 0xF7)) ] ]
            )
        , div [ class "button-container" ]
            (List.map (\operand -> button [ class "button operand-button", onClick <| AppendToOperand (toFloat operand) ] [ text (String.fromInt operand) ]) (List.range 4 6)
                ++ [ button [ class "button operation-button", onClick <| UpdateOperator Mult ] [ text (String.fromChar (Char.fromCode 0xD7)) ] ]
            )
        , div [ class "button-container" ]
            (List.map (\operand -> button [ class "button operand-button", onClick <| AppendToOperand (toFloat operand) ] [ text (String.fromInt operand) ]) (List.range 1 3)
                ++ [ button [ class "button operation-button", onClick <| UpdateOperator Sub ] [ text (String.fromChar (Char.fromCode 0x2212)) ] ]
            )
        , div [ class "button-container" ]
            [ button [ class "button operand-button", onClick <| AppendToOperand 0.0 ] [ text "0" ]
            , div [ class "button block" ] []
            , button [ class "button operation-button", onClick Calculate ] [ text (String.fromChar (Char.fromCode 0x3D)) ]
            , button [ class "button operation-button", onClick <| UpdateOperator Add ] [ text (String.fromChar (Char.fromCode 0x2B)) ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
