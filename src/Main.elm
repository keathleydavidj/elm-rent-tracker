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
  { name : String
  , split : Int
  }

type alias Bill =
  { description : String
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




model : Model
model = 
  { participants = [david]
  , entries = [water]
  , bill = billForm 
  , user = userForm
  }

david : User
david = User "David" 0

water : Bill
water = Bill "Water Bill" 12.34 [david]

billForm : Form
billForm = 
  [ Input "" "description" "what kinda bill you owe?" "text" Nothing
  , Input "" "amount" "Amount" "number" Nothing
  ]

userForm : Form
userForm =
  [ Input "" "name" "Name" "text" Nothing
  , Input "" "split" "% of rent" "number" Nothing
  ]

submitUser : Form -> Maybe User
submitUser frm =
  let
    isValid : Input -> Bool 
    isValid inp =
      case inp.error of
        Just str ->
          False
    formIsValid : Form -> Bool
    formIsValid frm =
      List.fold |> List.map isValid frm

    f = 
      \inp => {i.name = i.value}
    n =
  in
    List.map f frm


-- UPDATE


type Msg
  = NoOp 
  | OnInputChanged Input String
  | OnSubmitUser



update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model
      
    OnInputChanged inp newValue ->
      { model | bill = List.map (updateInput inp newValue) model.bill
              , user = List.map (updateInput inp newValue) model.user }

    OnSubmitUser -> model

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
    [ ( "description"
      , (ifEmpty "Please describe this bill")
      )
    , ( "amount"
      , (ifEmpty "Please enter an amount of this bill")
      )
    , ( "name"
      , (ifEmpty "Please enter a name")
      )
    , ( "split"
      , (ifEmpty "Please enter a percentage")
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


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div []
    [ div [] 
      [ h3 [] [ text "Bills" ]
      , formView model.bill 
      , billListView model.entries
      ]
    , div []
      [ h3 [] [ text "Users" ]
      , formView model.user 
      , userListView model.participants
      ]
    ]

formView : List Input -> Html Msg
formView frm =
  div [] (List.map inputView frm)

inputView : Input -> Html Msg
inputView inp =
  div []
    [ div [] [ input [placeholder inp.label, onInput (OnInputChanged inp), value inp.value] [] ]
    , div [] [ p [] [ text (withDefault "" inp.error) ] ]
    ]

userView : User -> Html Msg
userView user =
  li []
    [ p [] [ text <| "Name: " ++ user.name ]
    , p [] [ text <| "Percentage: " ++ (toString user.split) ]
    ]

userListView : List User -> Html Msg
userListView users =
  ul [] (List.map userView users)

billView : Bill -> Html Msg
billView bill =
  li []
    [ p [] [ text <| "Bill: " ++ bill.description ]
    , p [] [ text <| "Amount: $" ++ (toString bill.amount) ]
    ]

billListView : List Bill -> Html Msg
billListView bills =
  ul [] (List.map billView bills)
