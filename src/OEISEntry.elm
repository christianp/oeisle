module OEISEntry exposing (OEISEntry, Sequence, decode, get_entry, a_number, link)

import Array exposing (Array)
import Html as H
import Html.Attributes as HA
import Json.Decode as JD

type alias Sequence = Array Int

type alias OEISEntry = 
    { name : String
    , number : Int
    , data: Sequence
    }

decode : JD.Decoder OEISEntry
decode =
    JD.map3 OEISEntry
        (JD.field "name" JD.string)
        (JD.field "number" JD.int)
        (JD.field "seq" (JD.array JD.int))

get_entry : Int -> OEISEntry -> Int
get_entry i entry = Array.get i entry.data |> Maybe.withDefault 0

a_number : OEISEntry -> String
a_number entry = 
    let
        digits = String.padLeft 6 '0' (String.fromInt entry.number)
    in
        "A" ++ digits

link entry =
    let
        url = "https://oeis.org/" ++ (a_number entry)
    in
        H.a
            [ HA.href url 
            , HA.class "oeis-entry-link"
            , HA.target "oeis-entry"
            ]
            [ H.text <| (a_number entry) ++ " - " ++ entry.name ]
