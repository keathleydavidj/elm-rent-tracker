module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Date exposing ( Date )
import Time exposing ( Time )
import Task

-- component import example
-- import Components.Hello exposing ( hello )


-- APP
main : Program Never Int Msg
main =
  Html.Program 
    { init = ( Nothing, now )
    , model = model
    , view = view
    , update = update }


-- MODEL
type alias Model =
  { participants : List User
  , entries : List Bill
  , bill : Bill
  , user : User
  }

type alias User =
  { name : String
  , split : Int
  }

type alias Bill =
  { description : String
  , due : Date
  , amount : Float
  , paidBy : List User
  }

type Msg
  = NoOp 
  | SaveUser

model : Model
model = 
  { participants = []
  , entries = []
  , bill = newBill
  }

newBill : Bill
newBill =
  { description = ""
  , due = Date.now
  , amount = 0.0
  , paidBy = []
  }

newUser : User
newUser =
  { name = ""
  , split = 0
  }


-- UPDATE




update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model
      
    SaveUser ->
      { model | participants =
        participants ++ 
        [
          { name = bill.name
          , split = bill.split
          }
        ]
      }

now : Cmd Msg
now =
  Task.perform (always (SetDate Nothing)) (Just >> SetDate) Date.now
-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][    -- inline CSS (literal)
    div [ class "row" ][
      div [ class "col-xs-12" ][
        div [ class "jumbotron" ][
          , addUserForm model                                                                     -- ext 'hello' component (takes 'model' as arg)

          , p [] [ text ( "Add a Bill" ) ]
          , button [ class "btn btn-primary btn-lg", onClick Increment ] [                  -- click handler
            span[ class "glyphicon glyphicon-star" ][]                                      -- glyphicon
            , span[][ text "FTW!" ]
          ]
        ]
      ]
    ]
  ]

addBillForm : Model -> Html Msg
addBillForm model =
    form [ id "user-form" onSubmit SaveUser ] [
        h1 [] [ text "User Form" ],
        label [ for "name-field" ] [ text "name: " ],
        input [ id "name-field", type' "text", value model.user.name ] [],
        label [ for "percentage"] [text "percentage: " ],
        input [ id "percentage-field", type' "number", value model.user.split ] [],
        button [ class "user-button" ] [ text "Add User!" ]
    ]

-- CSS STYLES
styles : { img : List ( String, String ) }
styles =
  {
    img =
      [ ( "width", "33%" )
      , ( "border", "4px solid #337AB7")
      ]
  }
