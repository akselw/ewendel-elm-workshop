module Main exposing (main)

import Browser
import Deck exposing (Card, CardState(..), Deck, Group(..))
import GameState exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



--- Model


type Model
    = Initializing
    | Game GameState
    | GameOver GameOverState


type alias GameOverState =
    Int



--- Update


type Msg
    = CardClick Card
    | DeckGenerated Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardClick clickedCard ->
            case model of
                Game gameState ->
                    ( GameState.clickCard clickedCard gameState
                        |> Game
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DeckGenerated deck ->
            ( GameState.init deck
                |> Game
            , Cmd.none
            )


setCard : CardState -> Card -> Card
setCard cardState card =
    { card | state = cardState }



--- View


view : Model -> Html Msg
view model =
    case model of
        Initializing ->
            text ""

        Game gameState ->
            viewGame gameState

        GameOver gameOverState ->
            text "Game over"


viewGame : GameState -> Html Msg
viewGame gameState =
    div []
        [ gameState
            |> GameState.deck
            |> viewCards
        , viewScore gameState
        ]


viewScore : GameState -> Html msg
viewScore gameState =
    gameState
        |> GameState.score
        |> String.fromInt
        |> (++) "Score: "
        |> text


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
    ( Initializing, Random.generate DeckGenerated Deck.random )



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
