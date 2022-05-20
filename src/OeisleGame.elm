port module OeisleGame exposing (main)

import Array exposing (Array)
import Browser
import Html as H exposing (Html, div, button, ol, li, span, text, p, input)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Json.Decode as JD
import Json.Encode as JE
import OEISEntry exposing (OEISEntry, Sequence)
import Permutation as P exposing (Permutation)
import Random
import Random.Array
import Set exposing (Set)
import Tuple exposing (pair, first, second)

port loadEntry: () -> Cmd msg
port receiveEntry: (JE.Value -> msg) -> Sub msg

num_items = 10

type alias Game = 
    { turns: List Sequence
    , entry : OEISEntry
    , input : Permutation
    , moving_entry: Maybe Int
    }

type alias Model =
    { game : Maybe Game
    }

turn_valid game = Array.toList >> List.map String.toInt >> List.any ((==) Nothing) >> not

main = Browser.document
    { init = init
    , update = update
    , subscriptions = \_ -> receiveEntry (JD.decodeValue OEISEntry.decode >> ReceiveEntry)
    , view = view
    }

init_model =
    { game = Nothing
    }

make_game entry =
    { turns = []
    , entry = entry
    , input = P.identity
    , moving_entry = Nothing
    }

load_a_new_entry = loadEntry ()

init : () -> (Model, Cmd Msg)
init _ = (init_model, load_a_new_entry)

type Msg
    = ReceiveEntry (Result JD.Error OEISEntry)
    | SubmitTurn
    | Swap Int Int
    | PressEntry Int
    | SetInput Permutation
    | NewGame

nocmd : Model -> (Model, Cmd Msg)
nocmd model = (model, Cmd.none)

shuffle_input : Cmd Msg
shuffle_input =
    Random.generate SetInput (List.range 0 (num_items-1) |> Array.fromList |> Random.Array.shuffle)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveEntry res ->
            case (Debug.log "res" res) of
                Ok entry -> ({ game = Just (make_game entry) }, shuffle_input )
                Err _ -> model |> nocmd

        SetInput input ->
            affect_game (\game -> { game | input = input }) model |> nocmd

        SubmitTurn ->
            affect_game submit_turn model |> nocmd

        Swap a b ->
            affect_game (swap_pieces a b) model |> nocmd

        PressEntry i ->
            let
                q = Debug.log "press" i
            in
                affect_game (press_entry i) model |> nocmd

        NewGame -> (model, load_a_new_entry)

affect_game : (Game -> Game) -> Model -> Model
affect_game fn model = { model | game = Maybe.map fn model.game }

submit_turn game =
    let
        turn = List.range 0 (num_items-1) |> Array.fromList |> Array.map (P.map game.input >> \i -> OEISEntry.get_entry i game.entry)
    in
        { game | turns = turn::game.turns }

swap_pieces a b game = { game | input = P.compose (P.swap a b) game.input }

press_entry : Int -> Game -> Game
press_entry i game = 
    if game.moving_entry == Just i then
        { game | moving_entry = Nothing }
    else case game.moving_entry of
        Nothing ->
            { game | moving_entry = Just i }
        Just j ->
            { game | moving_entry = Nothing } |> swap_pieces i j

view model = case model.game of
    Nothing -> 
        { title = "OEISle"
        , body = [div [] []]
        }
    Just g -> 
        { title = "OEISLE"
        , body = [ view_game g, debug_text g ]
        }

debug_text game = 
    div
        []
        [ p [] [text <| Debug.toString game.input]
        , p [] [text <| Debug.toString game.moving_entry]
        , p [] [text <| Debug.toString (P.compose (P.swap 3 4) (P.swap 0 3))]
        ]

view_game game =
    div
        []
        [ view_sequence game.entry.data (Array.slice 0 num_items game.entry.data)
        , view_turns game
        , next_turn game
        , submit_button game
        , new_game_button
        ]

view_turns game = 
    ol
        [ HA.class "turns"
        ]
        (List.map (view_sequence game.entry.data) (List.reverse game.turns))

next_turn : Game -> Html Msg
next_turn game =
    ol
        [ HA.class "sequence next-turn"
        ]
        (List.map (next_turn_entry game) (List.range 0 (num_items-1)))

next_turn_entry : Game -> Int -> Html Msg
next_turn_entry game i =
    let
        n = Array.get (P.map game.input i) game.entry.data |> Maybe.withDefault 0
        moving = game.moving_entry == Just i
    in
    li
        [ HA.classList
            [ ("entry", True)
            , ("moving", moving)
            ]
        , HE.onClick (PressEntry i)
        ]
        [ text <| String.fromInt n]

submit_button game =
    button
        [ HA.class "submit"
        , HE.onClick SubmitTurn
        ]
        [ text "Submit" ]

new_game_button =
    button
        [ HA.class "submit"
        , HE.onClick NewGame
        ]
        [ text "New game" ]

view_sequence : Sequence -> Sequence -> Html Msg
view_sequence target_sequence sequence =
    let
        view_item guess target =
            let
                correct = guess == target
            in
                li 
                    [ HA.classList
                        [ ("guess", True)
                        , ("correct",correct)
                        ]
                    ] 
                    [  span [ HA.class "number" ] [text (String.fromInt guess)]
                    ]
    in
        ol 
            [ HA.class "sequence"
            ]
            (List.map2 view_item (Array.toList sequence) (Array.toList target_sequence))
