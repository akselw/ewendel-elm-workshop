module GameState exposing (GameState, clickCard, deck, init, score)

import Deck exposing (..)


type alias HighScore =
    Int


type alias GameInfo =
    { cards : Deck
    , highScores : List HighScore
    , pairsOpened : Int
    }


type GameState
    = Choosing GameInfo
    | Matching Card GameInfo
    | GameOver GameInfo


init : Deck -> GameState
init cards =
    Choosing { cards = cards, highScores = [], pairsOpened = 0 }


deck : GameState -> Deck
deck gameState =
    case gameState of
        Choosing { cards } ->
            cards

        Matching _ { cards } ->
            cards

        GameOver { cards } ->
            cards


score : GameState -> Int
score gameState =
    case gameState of
        Choosing { pairsOpened } ->
            pairsOpened

        Matching _ { pairsOpened } ->
            pairsOpened

        GameOver { pairsOpened } ->
            pairsOpened


clickCard : Card -> GameState -> GameState
clickCard clickedCard gameState =
    case gameState of
        Choosing gameInfo ->
            gameInfo.cards
                |> List.map closeOpenCard
                |> List.map
                    (\elem ->
                        if elem.id == clickedCard.id && elem.group == clickedCard.group then
                            setCard Open clickedCard

                        else
                            elem
                    )
                |> setCards gameInfo
                |> Matching clickedCard

        Matching matchingCard gameInfo ->
            if clickedCard.id == matchingCard.id && clickedCard.group /= matchingCard.group then
                setCardsWithId Matched clickedCard.id gameInfo.cards
                    |> setCards gameInfo
                    |> updateCount
                    |> gameStateAfterMatch

            else
                changeCardState Open clickedCard gameInfo.cards
                    |> setCards gameInfo
                    |> updateCount
                    |> Choosing

        GameOver cards ->
            gameState


updateCount : GameInfo -> GameInfo
updateCount gameInfo =
    { gameInfo | pairsOpened = gameInfo.pairsOpened + 1 }


setCards : GameInfo -> Deck -> GameInfo
setCards gameInfo cards =
    { gameInfo | cards = cards }


closeOpenCard : Card -> Card
closeOpenCard card =
    if card.state == Open then
        setCard Closed card

    else
        card


gameStateAfterMatch : GameInfo -> GameState
gameStateAfterMatch gameInfo =
    if List.any (\e -> e.state /= Closed) gameInfo.cards then
        Choosing { gameInfo | cards = gameInfo.cards }

    else
        GameOver { gameInfo | cards = gameInfo.cards }


changeCardState : CardState -> Card -> Deck -> Deck
changeCardState cardState card cards =
    cards
        |> List.map
            (\elem ->
                if elem.id == card.id && elem.group == card.group then
                    setCard cardState card

                else
                    elem
            )


setCardsWithId : CardState -> String -> Deck -> Deck
setCardsWithId cardState id cards =
    cards
        |> List.map
            (\elem ->
                if elem.id == id then
                    setCard cardState elem

                else
                    elem
            )


setCard : CardState -> Card -> Card
setCard cardState card =
    { card | state = cardState }
