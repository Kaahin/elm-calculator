module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String
import Char


type alias Model =
    { operand1 : String
    , operand2 : String
    , operator : String
    , result : String
    , upperDisplay : String
    , lowerDisplay : String
    }

init : Model
init = 
    { operand1 = ""
    , operand2 = ""
    , operator = ""
    , result = ""
    , upperDisplay = ""
    , lowerDisplay = "0"    
    }

type Msg
    = AppendToOperand String
    | Clear
    | ClearEntry
    | UpdateOperator String
    | Calculate

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        AppendToOperand operand ->
            if operand == "." && String.contains "." model.lowerDisplay then
                model
            else
                if model.operator == "" then
                    let updatedOperand1 = model.operand1 ++ operand
                    in { model | operand1 = updatedOperand1, lowerDisplay = updatedOperand1 }
                else
                    let updatedOperand2 = model.operand2 ++ operand
                    in { model | operand2 = updatedOperand2, lowerDisplay = updatedOperand2 }
            
        Clear -> 
            { operand1 = "", operand2 = "", operator = "", result = "", upperDisplay = "", lowerDisplay = "0" }

        ClearEntry -> 
            if model.result /=  "" then 
                { operand1 = "", operand2 = "", operator = "", result = "", upperDisplay = "", lowerDisplay = "0" }
            else if model.operator /= "" then
                { model | operand2 = "", operator = "", upperDisplay = model.operand1, lowerDisplay = "0"}
            else
                { model | operand1 = "", lowerDisplay = "0"}

        UpdateOperator op -> 
            { model | operator = op, upperDisplay = model.operand1 ++ " " ++ op }
        
        Calculate -> 
            let 
                o1 = String.toFloat model.operand1 |> Maybe.withDefault 0
                o2 = String.toFloat model.operand2 |> Maybe.withDefault 0
                res =
                    case model.operator of 
                        "+" -> 
                            String.fromFloat (o1 + o2)

                        "-" ->  
                            String.fromFloat (o1 - o2)
                        
                        "*" ->
                            String.fromFloat (o1 * o2)
                        
                        "/" ->
                            if o2 == 0 then
                                "Error: Division by zero"
                            else
                                String.fromFloat (o1 / o2)

                        _ ->
                            "Error: Unknown operation"
            in
            { model | result = res, lowerDisplay = res, upperDisplay = model.upperDisplay ++ " " ++ model.operand2 ++ " = " }

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
        , div [ class "button-container" ] (List.map (\operand -> button [ class "button operand-button", onClick <| AppendToOperand (String.fromInt operand) ] [ text (String.fromInt operand) ]) (List.range 7 9)
        ++ [ button [ class "button operation-button", onClick <| UpdateOperator "/"  ] [ text (String.fromChar (Char.fromCode 0x00F7)) ] ]
        )
        , div [ class "button-container" ] (List.map (\operand -> button [ class "button operand-button", onClick <| AppendToOperand (String.fromInt operand) ] [ text (String.fromInt operand) ]) (List.range 4 6)
        ++ [ button [ class "button operation-button", onClick <| UpdateOperator "*" ] [ text (String.fromChar (Char.fromCode 0x00D7)) ] ]
        )
        , div [ class "button-container" ] (List.map (\operand -> button [ class "button operand-button", onClick <| AppendToOperand (String.fromInt operand) ] [ text (String.fromInt operand) ]) (List.range 1 3)
        ++ [ button [ class "button operation-button", onClick <| UpdateOperator "-" ] [ text (String.fromChar (Char.fromCode 0x2212)) ] ]        
        )
        , div [ class "button-container" ] 
            [ button [ class "button operand-button", onClick (AppendToOperand "0") ] [ text "0" ]
            , button [ class "button dot-button", onClick (AppendToOperand ".") ] [ text "." ]
            , button [ class "button operation-button", onClick Calculate ] [ text (String.fromChar (Char.fromCode 0x003D)) ]
            , button [ class "button operation-button", onClick <| UpdateOperator "+" ] [ text (String.fromChar (Char.fromCode 0x002B)) ]
            ]
        ]

main : Program () Model Msg
main = 
    Browser.sandbox { init = init, update = update, view = view }
                     

