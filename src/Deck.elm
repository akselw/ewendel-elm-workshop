module Deck exposing (Card, CardState(..), Deck, Group(..), random, static)

import Random
import Random.List


type alias Deck =
    List Card


type alias Card =
    { id : String
    , state : CardState
    , group : Group
    }


type CardState
    = Open
    | Closed
    | Matched


type Group
    = A
    | B


random : Random.Generator Deck
random =
    static
        |> Random.List.shuffle


static : Deck
static =
    let
        urls =
            [ "1"
            , "2"
            , "3"
            , "4"
            , "5"
            , "6"
            ]

        groupA =
            urls |> List.map (\url -> { id = url, group = A, state = Closed })

        groupB =
            urls |> List.map (\url -> { id = url, group = B, state = Closed })
    in
    List.concat [ groupA, groupB ]
