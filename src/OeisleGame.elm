port module OeisleGame exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as BE
import Html as H exposing (Html, div, button, ul, ol, li, span, text, p, input)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Json.Decode as JD
import Json.Encode as JE
import OEISEntry exposing (OEISEntry, Sequence)
import Set exposing (Set)
import Tuple exposing (pair, first, second)
import Url.Builder

port loadEntry: (String, Maybe (List Int), Bool) -> Cmd msg
port receiveEntry: (JE.Value -> msg) -> Sub msg

num_items = 5

type Screen
    = HelpScreen
    | GameScreen

type CantSubmitReason
    = NotLongEnough
    | NotASequence

type ReceivedEntry
    = ReceiveTargetSequence (Maybe Int) OEISEntry
    | ReceiveQueryResults (List OEISEntry)

type alias Turn =
    { sequence : Sequence
    , matched_sequences : List OEISEntry
    }

type alias Game = 
    { turns: List Turn
    , entry : OEISEntry
    , input : List Int
    , matched_sequences : Maybe (List OEISEntry)
    , edition : Maybe Int
    }

type alias Model =
    { game : Maybe Game
    , screen : Screen
    }

game_finished game = (List.head >> Maybe.map (.sequence)) game.turns == Just (Array.slice 0 num_items game.entry.data)

decode_is_string : String -> String -> JD.Decoder a -> JD.Decoder a
decode_is_string field s decode = 
    JD.field field JD.string |> JD.andThen (\v ->
        if v == s then
            decode
        else
            JD.fail <| "Expected to see \"" ++ s ++ "\" in field \"" ++ field ++ "\""
    )

decode_received_entry = 
    JD.oneOf
        [ decode_target_entry
        , decode_query_response
        ]

decode_target_entry = 
    decode_is_string "reason" "target" (JD.map2 ReceiveTargetSequence (JD.field "edition" (JD.maybe JD.int)) (JD.field "entry" OEISEntry.decode))

decode_query_response = decode_is_string "reason" "query" (JD.map ReceiveQueryResults (JD.field "matches" (JD.list OEISEntry.decode)))

decode_keypress = JD.field "key" JD.string |> JD.andThen (\key ->
        case key of
            "Backspace" -> JD.succeed Backspace
            "Enter" -> JD.succeed SubmitTurn
            _ -> 
                case String.toInt key of
                    Just n -> JD.succeed (TypeDigit n)
                    _ -> JD.fail ""
    )

main = Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions model =
    Sub.batch
        [ receiveEntry (JD.decodeValue decode_received_entry >> ReceiveEntry)
        , BE.onKeyUp decode_keypress
        ]

init_model =
    { game = Nothing
    , screen = HelpScreen
    }

make_game edition entry =
    { turns = []
    , entry = entry
    , input = []
    , matched_sequences =  Nothing
    , edition = edition
    }

load_a_new_entry random = loadEntry ("target", Nothing, random)

fetch_entry sequence = loadEntry ("query",Just sequence,False)

init : () -> (Model, Cmd Msg)
init _ = (init_model, load_a_new_entry False)

type Msg
    = ReceiveEntry (Result JD.Error ReceivedEntry)
    | SubmitTurn
    | TypeDigit Int
    | Backspace
    | NewGame
    | SetScreen Screen

nocmd : Model -> (Model, Cmd Msg)
nocmd model = (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveEntry res ->
            case res of
                Ok reason ->
                    case reason of
                        ReceiveTargetSequence edition entry -> { model | game = Just (make_game edition entry) } |> nocmd
                        ReceiveQueryResults matches -> affect_game (\g -> { g | matched_sequences = Just matches }) model |> nocmd
                Err _ -> model |> nocmd

        SubmitTurn ->
            if Maybe.map can_submit model.game |> Maybe.withDefault False then
                affect_game submit_turn model |> nocmd
            else
                nocmd model

        TypeDigit i ->
            affect_game_cmd (type_digit i) model

        Backspace ->
            affect_game type_backspace model |> nocmd

        NewGame -> (model, load_a_new_entry True)

        SetScreen screen -> { model | screen = screen } |> nocmd

affect_game : (Game -> Game) -> Model -> Model
affect_game fn model = { model | game = Maybe.map fn model.game }

affect_game_cmd : (Game -> (Game, Cmd Msg)) -> Model -> (Model, Cmd Msg)
affect_game_cmd fn model = 
    case model.game of
        Nothing -> (model, Cmd.none)
        Just game ->
            let
                (ngame, cmd) = fn game
            in
                ( { model | game = Just ngame }, cmd )

cant_submit_reason : Game -> Maybe CantSubmitReason
cant_submit_reason game =
    if List.length game.input /= num_items then
        Just NotLongEnough
    else if not (Maybe.map ((/=) []) game.matched_sequences |> Maybe.withDefault False) then
        Just NotASequence
    else
        Nothing

can_submit game = cant_submit_reason game == Nothing

submit_turn game =
    case (can_submit game, game.matched_sequences) of
        (True, Just sequences) -> 
            let
                turn = 
                    { sequence = Array.fromList game.input
                    , matched_sequences = sequences
                    }
            in
                { game | turns = turn::game.turns, matched_sequences = Nothing, input = [] }
        _ -> game

type_digit i game =
    if List.length game.input == num_items then
        (game, Cmd.none)
    else
        let
            ngame = { game | input = game.input ++ [i] }
            cmd = if List.length ngame.input == num_items then fetch_entry ngame.input else Cmd.none
        in
            (ngame, cmd)

type_backspace game =
    { game | input = List.take ((List.length game.input) - 1) game.input, matched_sequences = Nothing }

view model = case model.game of
    Nothing -> 
        { title = "OEISle"
        , body = [div [] []]
        }
    Just g -> 
        { title = "OEISle"
        , body = 
            [ header model
            , view_screen model g
            --, debug_text g 
            , footer
            ]
        }

view_screen model game
    = case model.screen of
        HelpScreen -> view_help
        GameScreen -> view_game game


header model =
    H.node "header"
        []
        [ H.h1 [] [ text "OEISle" ]
        , let
            (screen, label) = 
                if model.screen == HelpScreen then
                    (GameScreen, "×")
                else
                    (HelpScreen, "?")
          in
              button 
                (SetScreen screen) 
                label 
                [ HA.class "help-link" 
                , HA.title "Help"
                ]
        , case model.game of
            Just game -> case game.edition of
                Just edition -> para <| "No. " ++ (String.fromInt edition)
                Nothing -> para "???"
            Nothing -> text ""
        ]

debug_text game = 
    div
        []
        [ para <| if game_finished game then "finished" else "guessing"
        , para <| Debug.toString game.matched_sequences
        ]

para s = p [] [text s]

external_link url label =
    H.a
        [ HA.href url
        , HA.target "_blank" 
        ]
        [ text label ]

view_help =
    H.main_
        [ HA.class "help" ]
        [ p 
            []
            [ text "I'm thinking of an integer sequence from the "
            , external_link "https://oeis.org" "Online Encyclopedia of Integer Sequences"
            , text "."
            ]
        , para "You have to guess the first five terms."
        , para "Here are some clues:"
        , ul
            []
            [ li 
                [] 
                [ text "It's a "
                , external_link "https://oeis.org/search?q=keyword%3Anice" "nice"
                , text " sequence."
                ]
            , li
                []
                [ text "All of the first five terms are between 0 and 9." ]
            ]
        , para "Each of your guesses must be the start of a valid sequence."
        , para "After each guess, you're told if each entry is:"
        , ul
            [ HA.class "no-bullets" ]
            [ li [] [H.strong [] [ text "✓" ], text " in the right place" ]
            , li [] [H.strong [] [ text "↔" ], text " in the first five terms but not in the right place" ]
            , li [] [H.strong [] [ text "⇒" ], text " in the sequence but not one of the first five terms" ]
            , li [] [H.strong [] [ text "✗" ], text " not in the sequence." ]
            ]
        , button (SetScreen GameScreen) "Start playing" [ HA.class "start-game" ]
        ]


view_game game =
    H.main_
        []
        (
            [ view_turns game ]
            ++
            (if game_finished game then 
                [ result game
                , para "See you again tomorrow!"
                , new_game_button
                ]
             else
                [ view_next_turn game
                , hint game
                , keypad game
                ]
            )
        )

hint game = 
    let
        hint_text = case cant_submit_reason game of
            Just NotASequence -> "That's not a sequence I know"
            Just NotLongEnough -> "Write a sequence"
            Nothing -> "Try it!"
    in
        p 
            [ HA.class "hint" ] 
            [ text hint_text ]

view_turns game = 
    ol
        [ HA.class "turns"
        ]
        (List.map (\turn -> li [] [view_turn game.entry.data turn]) (List.reverse game.turns))

view_next_turn : Game -> Html Msg
view_next_turn game =
    ol
        [ HA.class "sequence next-turn"
        ]
        (List.map (next_turn_entry game) (List.range 0 (num_items-1)))

next_turn_entry : Game -> Int -> Html Msg
next_turn_entry game i =
    let
        n = Array.get i (Array.fromList game.input) |> Maybe.map String.fromInt |> Maybe.withDefault ""
    in
    li
        [ HA.classList
            [ ("guess", True)
            ]
        ]
        [ text n ]

button onClick label attrs =
    H.button
        ([ HE.onClick onClick ]++attrs)
        [ text label ]

keypad game =
    let
        submit_button =
            button SubmitTurn "⏎"
                [ HA.class "submit"
                , HA.disabled (not (can_submit game))
                , HA.title "Submit"
                ]
        digit i =
            button (TypeDigit i) (String.fromInt i)
                [ HA.class "digit"
                ]
        backspace =
            button Backspace "⌫"
               [ HA.class "backspace"
               , HA.title "Delete"
               ]
    in
        div 
            [ HA.class "keypad" ]
            ((List.map digit [7,8,9,4,5,6,0,1,2,3])++[backspace, submit_button])

new_game_button =
    button NewGame "Play again with a random sequence"
        [ HA.class "new-game"
        ]

view_turn : Sequence -> Turn -> Html Msg
view_turn target_sequence turn =
    let
        view_item guess target =
            let
                correct = guess == target
                somewhere = List.any ((==) guess) ((Array.toList >> List.take num_items) target_sequence)
                later = List.any ((==) guess) ((Array.toList) target_sequence)
                title = 
                    if correct then
                        "This number is correct"
                    else if somewhere then
                        "This number appears somewhere else in the first " ++ (String.fromInt num_items) ++ " entries"
                    else if later then
                        "This number appears somewhere in the sequence, after the first " ++ (String.fromInt num_items) ++ " entries"
                    else
                        "This number does not appear in the sequence."
            in
                li 
                    [ HA.classList
                        [ ("guess", True)
                        , ("incorrect", not later)
                        , ("correct",correct)
                        , ("somewhere",somewhere)
                        , ("later", later)
                        ]
                    , HA.title title
                    ] 
                    [  span [ HA.class "number" ] [text (String.fromInt guess)]
                    ]
    in
        ol 
            [ HA.class "sequence"
            ]
            ((List.map2 view_item (Array.toList turn.sequence) (Array.toList target_sequence))++[view_matched_sequences turn.sequence turn.matched_sequences])

view_matched_sequences sequence sequences = 
    case List.head sequences of 
        Nothing -> li [] []
        Just entry ->
            let
                n = List.length sequences
                search_url = 
                    Url.Builder.crossOrigin
                        "https://oeis.org"
                        [ "search" ]
                        [ Url.Builder.string "q" ((String.join "," ((Array.toList >> List.map String.fromInt) sequence))++" keyword:nice") ]
            in
                li
                    [ HA.class "matched-sequences" ]
                    ([ OEISEntry.link entry]
                     ++
                     (
                        if n==1 then 
                            []
                        else
                            [ text " and "
                            , H.a
                                [ HA.href <| search_url
                                , HA.target "oeis-entry"
                                ]
                                [ text <| (String.fromInt (n-1)) ++ " " ++ (if n==2 then "other" else "others") ]
                            ]
                     )
                    )
result game = 
    div
        [ HA.class "finished" ]
        (
            [ para "You found it!"
            , p
                []
                [ OEISEntry.link game.entry ]
            ]
            ++
            ( if game.edition == Nothing then
                []
              else
                [ para "Share your results:"
                , H.pre
                    [ HA.class "results-summary" ]
                    [ text <| success_summary game ]
                ]
            )
        )

success_summary game =
    String.join 
        "\n"
        [ short_success_summary game
        , success_summary_block game
        , "https://somethingorotherwhatever.com/oeisle"
        ]

short_success_summary game =
    let
        num_turns = List.length game.turns
        edition = Maybe.withDefault 0 game.edition
    in
        "I solved #OEISle "++(String.fromInt edition)++" in "++(String.fromInt num_turns)++"!"

success_summary_block game =
    let
        target_sequence = game.entry.data
        turn_summary turn = 
            String.join "" <| List.map2 view_item (Array.toList turn.sequence) (Array.toList target_sequence)
        view_item guess target =
            let
                correct = guess == target
                somewhere = List.any ((==) guess) ((Array.toList >> List.take num_items) target_sequence)
                later = List.any ((==) guess) ((Array.toList) target_sequence)
            in
                if correct then "✓"
                else if somewhere then "↔"
                else if later then "⇒"
                else "✗"

    in
        String.join "\n" ((List.reverse >> List.map turn_summary) game.turns)

footer =
    H.footer
        []
        [ p
            []
            [ text "Made by "
            , external_link "https://somethingorotherwhatever.com" "clp"
            ]
        , p
            []
            [ external_link "about.html" "More about this"
            , text "."
            ]
        ]
