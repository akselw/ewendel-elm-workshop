module Main exposing (main)

import Browser
import Deck exposing (Card, CardState(..), Deck, Group(..))
import GameState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



--- Model


type alias Model =
    GameState



--- Update


type Msg
    = CardClick Card
    | DeckGenerated Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardClick clickedCard ->
            ( GameState.clickCard clickedCard model, Cmd.none )

        DeckGenerated deck ->
            ( Choosing deck, Cmd.none )


setCard : CardState -> Card -> Card
setCard cardState card =
    { card | state = cardState }



--- View


view : Model -> Html Msg
view model =
    model
        |> GameState.deck
        |> viewCards


viewCards : Deck -> Html Msg
viewCards cards =
    cards
        |> List.map viewCard
        |> div [ class "cards" ]


viewCard : Card -> Html Msg
viewCard card =
    div []
        [ case card.state of
            Open ->
                img [ src ("/cats/" ++ card.id ++ ".png"), class "open" ] []

            Closed ->
                img [ src "/cats/closed.png", class "closed", onClick (CardClick card) ] []

            Matched ->
                img [ src ("/cats/" ++ card.id ++ ".png"), class "matched" ] []
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Choosing Deck.static, Random.generate DeckGenerated Deck.random )



--
--myCards : Deck
--myCards =
--    [ { id = "1"
--      , state = Closed
--      , group = A
--      }
--    , { id = "2"
--      , state = Closed
--      , group = A
--      }
--    , { id = "3"
--      , state = Closed
--      , group = A
--      }
--    ]
