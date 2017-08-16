module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput )
import Dict exposing (Dict)

import Maybe exposing (Maybe, withDefault)


-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram 
    { model = model
    , view = view
    , update = update }


-- MODEL
type alias Model =
  { participants : List User
  , entries : List Bill
  , bill : Form
  , user : Form
  }

type alias User =
  { id : Int
  , name : String
  , split : Int
  }

type alias Bill =
  { id : Int
  , description : String
  , amount : Float
  , paidBy : List User
  }

type alias Form = List Input

type alias Input =
  { value : String
  , name : String
  , label : String
  , inputType : String
  , error : Maybe String
  }

type alias Validation = Input -> Maybe String

validations : Dict String Validation
validations =
  let
    ifEmpty : String -> (Input -> Maybe String)
    ifEmpty error =
      \i -> 
        if i.value == "" then 
          Just error
        else
          Nothing
  in
  Dict.fromList
    [ ( "bill_description"
      , (ifEmpty "Please describe this bill")
      )
    , ( "bill_amount"
      , (ifEmpty "Please enter an amount of this bill")
      )
    ]


validateInput : Input -> Maybe String
validateInput inp =
  let
    validation =
      Dict.get inp.name validations
  in
    case validation of
      Just v ->
        v inp
      Nothing ->
        Nothing



model : Model
model = 
  { participants = [david]
  , entries = [water]
  , bill = buttForm 
  , user = []
  }


david = User 1 "David" 0

water = Bill 0 "Water Bill" 12.34 [david]

buttForm = 
  [ Input "" "bill_description" "what kinda bill" "text" Nothing
  , Input "" "bill_amount" "Amount" "number" Nothing
  ]


-- UPDATE


type Msg
  = NoOp 
  | OnInputChanged Input String



update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model
      
    OnInputChanged inp newValue ->
      { model | bill = List.map (updateInput inp newValue) model.bill }

updateInput : Input -> String -> Input -> Input
updateInput changedInput newValue storedInput =
  if changedInput == storedInput then
    let
      updatedInput =
        { changedInput | value = newValue }
    in
      { updatedInput | error = validateInput changedInput }

  else
    storedInput


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div [] [ formView model.bill ]

formView : List Input -> Html Msg
formView frm =
  div [] (List.map inputView frm)

inputView : Input -> Html Msg
inputView inp =
  div []
    [ div [] [ input [placeholder inp.label, onInput (OnInputChanged inp), value inp.value] [] ]
    , div [] [ p [] [ text (withDefault "" inp.error) ] ]
    ]
