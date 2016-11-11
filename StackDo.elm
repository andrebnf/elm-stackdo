port module StackDo exposing (..)

import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task
import Debug


{-|
  StackDo App
    based on Elm Todo MVC available on https://github.com/evancz/elm-todomvc
-}
main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { tasks : List Task
    , field : String
    , uid : Int
    }


type alias Task =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


emptyModel : Model
emptyModel =
    { tasks = []
    , field = ""
    , uid = 0
    }


newTask : String -> Int -> Task
newTask desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


{-| Defining the possible actions of the app
-}
type Msg
    = NoOp
    | Add
    | UpdateField String
    | EditingTask Int Bool
    | UpdateTask Int String
    | Delete Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
                | uid = model.uid + 1
                , field = ""
                , tasks =
                    if String.isEmpty model.field then
                        model.tasks
                    else
                        [ newTask model.field model.uid ] ++ model.tasks
            }
                ! []

        UpdateField str ->
            { model | field = str }
                ! []

        EditingTask id isEditing ->
            let
                updateTask t =
                    if t.id == id then
                        { t | editing = isEditing }
                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ toString id)
            in
                { model | tasks = List.map updateTask model.tasks }
                    ! []

        UpdateTask id task ->
            let
                updateTask t =
                    if t.id == id then
                        { t | description = task }
                    else
                        t
            in
                { model | tasks = List.map updateTask model.tasks }
                    ! []

        Delete id ->
            { model | tasks = List.filter (\t -> t.id /= id) model.tasks }
                ! []



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "stack-do-wrapper" ]
        [ section
            [ class "stackdo-app" ]
            [ lazy viewInput model.field
            , lazy viewTasks model.tasks
            ]
        , div
            [ class "footer" ]
            [ text "created by "
            , a [ href "http://andrebnf.github.io/", target "_blank" ] [ text "André Bonfatti" ]
            , text " based on "
            , a [ href "https://github.com/evancz/elm-todomvc", target "_blank" ] [ text "TodoMVC" ]
            ]
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 []
            [ img [ src "logo.png" ] []
            , text "stackdo"
            ]
        , input
            [ class "new-todo"
            , placeholder "Something new to be done"
            , autofocus True
            , value task
            , name "newTask"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)



-- VIEW ALL TASKS


viewTasks : List Task -> Html Msg
viewTasks tasks =
    section
        [ class "main" ]
        [ Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedTask tasks
        ]


viewKeyedTask : Task -> ( String, Html Msg )
viewKeyedTask todo =
    ( toString todo.id, lazy viewTask todo )


viewTask : Task -> Html Msg
viewTask todo =
    li
        [ classList [ ( "editing", todo.editing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "edit"
                , value todo.description
                , name "title"
                , id ("todo-" ++ toString todo.id)
                , onClick (EditingTask todo.id True)
                , onInput (UpdateTask todo.id)
                , onBlur (EditingTask todo.id False)
                , onEnter (EditingTask todo.id False)
                ]
                []
            , button
                [ class "destroy"
                , onClick (Delete todo.id)
                ]
                []
            ]
        ]
