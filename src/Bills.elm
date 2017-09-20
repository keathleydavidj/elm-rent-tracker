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
import Material.List as Lists
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
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
    { amount : Float
    , description : String
    , due : String
    , editing : Bool
    , paid : Bool
    }


initBill : Bill
initBill =
    { amount = 0
    , description = "Water"
    , due = "07/04/1776"
    , editing = True
    , paid = False
    }



-- ACTION, UPDATE


type Msg
    = Add
    | UpdateAmount Int String
    | UpdateDesc Int String
    | UpdateDue Int String
    | ToggleEdit Int
    | TogglePaid Int
    | Delete Int
    | Mdl (Material.Msg Msg)


mapWithIndex : Int -> (a -> a) -> Array a -> Array a
mapWithIndex k f a =
    Array.get k a
        |> Maybe.map (\x -> Array.set k (f x) a)
        |> Maybe.withDefault a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAmount k num ->
            pure { model | bills = mapWithIndex k (setAmount num) model.bills }

        UpdateDesc k str ->
            pure { model | bills = mapWithIndex k (setDescription str) model.bills }

        UpdateDue k str ->
            pure { model | bills = mapWithIndex k (setDue str) model.bills }

        ToggleEdit k ->
            pure { model | bills = mapWithIndex k toggleEdit model.bills }

        TogglePaid k ->
            pure { model | bills = mapWithIndex k togglePaid model.bills }

        Add ->
            pure { model | bills = Array.push initBill model.bills }

        Delete idx ->
            pure { model | bills = deleteBill idx model.bills }

        Mdl msg_ ->
            Material.update Mdl msg_ model


setAmount : String -> Bill -> Bill
setAmount newValue bill =
    case String.toFloat newValue of
        Err msg ->
            bill

        Ok amount ->
            { bill | amount = amount }


setDescription : String -> Bill -> Bill
setDescription newValue bill =
    { bill | description = newValue }


setDue : String -> Bill -> Bill
setDue newValue bill =
    { bill | due = newValue }


toggleEdit : Bill -> Bill
toggleEdit bill =
    { bill | editing = not bill.editing }


togglePaid : Bill -> Bill
togglePaid bill =
    { bill | paid = not bill.paid }


deleteBill : Int -> Array Bill -> Array Bill
deleteBill idx bills =
    removeFromArray idx bills


removeFromList : Int -> List a -> List a
removeFromList i xs =
    (List.take i xs) ++ (List.drop (i + 1) xs)


removeFromArray : Int -> Array a -> Array a
removeFromArray i =
    Array.toList >> removeFromList i >> Array.fromList



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    viewTable model


viewTable : Model -> Html Msg
viewTable model =
    let
        bills =
            model.bills
                |> Array.toList
                |> List.indexedMap viewTableRow
    in
        Table.table []
            [ Table.thead []
                [ Table.tr []
                    [ Table.th [] [ text "Description" ]
                    , Table.th [] [ text "Due Date" ]
                    , Table.th [] [ text "Amount" ]
                    ]
                ]
            , Table.tbody [] bills
            ]


viewTableRow : Int -> Bill -> Html Msg
viewTableRow idx bill =
    Table.tr []
        [ Table.td [] [ text bill.description ]
        , Table.td [] [ text bill.due ]
        , Table.td [ Table.numeric ] [ text <| toString bill.amount ]
        ]



-- view : Model -> Html Msg
-- view model =
--     let
--         bills =
--             model.bills
--                 |> Array.toList
--                 |> List.indexedMap viewBill
--     in
--         List.concatMap identity
--             [ [ Button.render Mdl
--                     [ 1 ]
--                     model.mdl
--                     [ Options.onClick Add ]
--                     [ text "Add Bill" ]
--               ]
--             , bills
--             ]
--             |> Lists.ul [ css "margin" "0", css "padding" "0" ]
--             |> Material.Scheme.top
--
--
-- viewBill : Int -> Bill -> Html Msg
-- viewBill idx bill =
--     case bill.editing of
--         False ->
--             seeBill idx bill
--
--         True ->
--             editBill idx bill
--
--
-- seeBill : Int -> Bill -> Html Msg
-- seeBill idx bill =
--     Lists.li [ Lists.withSubtitle ]
--         [ Lists.content []
--             [ Lists.icon "cake" []
--             , text bill.description
--             , Lists.subtitle [] [ text bill.due ]
--             ]
--         , Lists.content2 []
--             [ text <| "$" ++ toString bill.amount
--             , Toggles.switch Mdl
--                 [ 0 ]
--                 model.mdl
--                 [ Options.onToggle (ToggleEdit idx)
--                 , Toggles.ripple
--                 , Toggles.value bill.editing
--                 ]
--                 [ text "Edit" ]
--             ]
--         ]
--
--
-- editBill : Int -> Bill -> Html Msg
-- editBill idx bill =
--     Lists.li [ Lists.withSubtitle ]
--         [ Lists.content []
--             [ Lists.icon "cake" []
--             , Textfield.render Mdl
--                 [ 2 ]
--                 model.mdl
--                 [ Textfield.label "Description"
--                 , Textfield.floatingLabel
--                 , Textfield.text_
--                 , Textfield.value bill.description
--                 , Options.onInput (UpdateDesc idx)
--                 ]
--                 []
--             , Textfield.render Mdl
--                 [ 3 ]
--                 model.mdl
--                 [ Textfield.label "Due date"
--                 , Textfield.floatingLabel
--                 , Textfield.text_
--                 , Textfield.value bill.due
--                 , Options.onInput (UpdateDue idx)
--                 ]
--                 []
--             , Textfield.render Mdl
--                 [ 4 ]
--                 model.mdl
--                 [ Textfield.label "Amount"
--                 , Textfield.floatingLabel
--                 , Textfield.text_
--                 , Textfield.value <| toString bill.amount
--                 , Options.onInput (UpdateAmount idx)
--                 ]
--                 []
--             , Button.render Mdl
--                 [ 5 ]
--                 model.mdl
--                 [ Options.onClick (Delete idx) ]
--                 []
--             , Toggles.switch Mdl
--                 [ 0 ]
--                 model.mdl
--                 [ Options.onToggle (ToggleEdit idx)
--                 , Toggles.ripple
--                 , Toggles.value bill.editing
--                 ]
--                 [ text "Edit" ]
--             ]
--         ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
