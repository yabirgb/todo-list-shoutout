import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task

main : Program (Maybe Model) Maybe Msg

main =
    Browser.document
        { init = init
        , view = \model -> {tile = "Elm - todo", body = [view model]}
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }

port setStorage : Model -> Cmd msg

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [setStorage newModel, cmds]
        )
        

-- Create our model

type alias Model =
    { todos : List Todo
    , field : String
    , uid : Int
    , visibility : String
    }

type alias Todo =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }

emptyModel : Model
emptyModel =
    { todos = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }

newTodo : String -> Int -> Entry
newTodo desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }

init : Maybe Model -> (Model, Cmd Msg)
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )

-- Update

type Msg
    = NoOp
    | UpdateField String
    | EditingField Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String

-- Update a model depending on msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Add ->
            ( { model
                  | uid = model.uid + 1
                  , field = ""
                  , entries =
                      if String.isEmpty mode.field then
                          model.todos
                      else
                          model.todos ++ [newTodo model.field model.uid]
              }
            , Cmd.none
  
            )

        UpdateField str ->
            ( { model | field = str}
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }
                    else
                        t

                focus =
                    Dom.focus ("todo-"++String.fromInt id)
            in
                ( {model | todos = List.map UpdateEnty model.todos}
                , Task.attemp (\_ -> NoOp) focus
                )
        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }
                    else
                        t
            in
            ( { model | todos = List.map updateEntry model.todos }
            , Cmd.none
            )

        Delete id ->
            ( { model | todos = List.filter (\t -> t.id /= id) model.todos}
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | todos = List.filter (not << .completed) model.entries}
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted}
                    else
                        t
            in
            ( { model | entries = List.map updateEntryy model.entries}
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )


-- Views

view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
              [ class "todoapp" ]
              [ lazy viewInput model.field
              , lazy2 viewWntries model.visibility model.entries
              ,  lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        ]
