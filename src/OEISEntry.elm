module OEISEntry exposing (OEISEntry, Sequence, decode, get_entry)

import Array exposing (Array)
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
