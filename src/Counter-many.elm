{- This file re-implements the Elm Counter example (many counters) with
   elm-mdl. Look at this file if you have a dynamic number of elm-mdl components
   and are unsure how to choose proper indices.

   If you are looking for a starting point, you want `Counter.elm` rather than
   this file.
-}


module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Array exposing (Array)
import Material
import Material.Scheme
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Helpers exposing (pure)


-- MODEL


type alias Model =
    { bills : Array Bill
    , mdl : Material.Model
    }


model : Model
model =
    { bills = Array.empty
    , mdl = Material.model
    }


type alias Bill =
    { amount : Int
    , description : String
    , due : String
    }


initBill : Bill
initBill =
    { amount = 0
    , description = "Water"
    , due = "07/04/1776"
    }



-- ACTION, UPDATE


type Msg
    = Add
    | UpdateAmount Int Int
    | UpdateDesc Int String
    | UpdateDue Int String
    | Remove Int
    | Mdl (Material.Msg Msg)


map : Int -> (a -> a) -> Array a -> Array a
map k f a =
    Array.get k a
        |> Maybe.map (\x -> Array.set k (f x) a)
        |> Maybe.withDefault a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAmount k num ->
            pure { model | bills = map k (setAmount num) model.bills }

        UpdateDesc k str ->
            pure { model | bills = map k (setDescription str) model.bills }

        UpdateDue k str ->
            pure { model | bills = map k (setDue str) model.bills }

        Add ->
            pure { model | bills = Array.push initBill model.bills }

        Remove idx ->
            pure { model | bills = Array.slice 0 (idx - 1) model.bills ++ Array.slice (idx + 1) model.bills }

        Mdl msg_ ->
            Material.update Mdl msg_ model


setAmount : Int -> r -> r
setAmount num record =
    { record | amount = num }


setDescription : String -> r -> r
setDescription str record =
    { record | description = str }


setDue : String -> r -> r
setDue str record =
    { record | due = str }



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    let
        bills =
            model.bills
                |> Array.toList
                |> List.indexedMap viewBill
    in
        List.concatMap identity
            [ [ Button.render Mdl
                    [ 2 ]
                    model.mdl
                    [ Options.onClick Add ]
                    [ text "Add counter" ]
              ]
            , bills
            ]
            |> div []
            |> Material.Scheme.top


viewBill : Int -> Bill -> Html Msg


input : Html msg
input =
    input
        [ style [ ( "padding", "2rem" ) ] ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
