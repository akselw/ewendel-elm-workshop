module GameState exposing (GameState(..), clickCard, deck)

import Deck exposing (..)


type GameState
    = Choosing Deck
    | Matching Card Deck
    | GameOver Deck


deck : GameState -> Deck
deck gameState =
    case gameState of
        Choosing cards ->
            cards

        Matching _ cards ->
            cards

        GameOver cards ->
            cards


clickCard : Card -> GameState -> GameState
clickCard clickedCard gameState =
    case gameState of
        Choosing cards ->
            cards
                |> List.map closeOpenCard
                |> List.map
                    (\elem ->
                        if elem.id == clickedCard.id && elem.group == clickedCard.group then
                            setCard Open clickedCard

                        else
                            elem
                    )
                |> Matching clickedCard

        Matching matchingCard cards ->
            if clickedCard.id == matchingCard.id && clickedCard.group /= matchingCard.group then
                setCardsWithId Matched clickedCard.id cards
                    |> gameStateAfterMatch

            else
                changeCardState Open clickedCard cards
                    |> Choosing

        GameOver cards ->
            gameState


closeOpenCard : Card -> Card
closeOpenCard card =
    if card.state == Open then
        setCard Closed card

    else
        card


gameStateAfterMatch : Deck -> GameState
gameStateAfterMatch cards =
    if List.any (\e -> e.state /= Closed) cards then
        Choosing cards

    else
        GameOver cards


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
