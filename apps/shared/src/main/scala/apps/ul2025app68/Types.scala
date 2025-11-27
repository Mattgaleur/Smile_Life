package apps.ul2025app68

import cs214.webapp.UserId

enum Card:
    case Flirt
    case Child
    case Money
    case Profession
    case Study
    case Pet
    case Malus
    case Special


type Hand = Vector[Card]

type PlayedHand = // Or type Life ? Or type Deck ?
    Vector[Card]

type Board = Map[UserId, PlayedHand]

enum CardPile:
    case DefaultPile(cards: List[Card]) // A Stack would be better ?
    case DiscardPile(cards: List[Card])

enum Event:
    case Discard(card: Card)
    case PlayCard(card: Card)
    case PickCard(isDefaultPile: Boolean)
        // true -> DefaultPile
        // false -> DiscardPile

case class View(
    board: Board,
    hand: Hand
)

case class State(
    hands: Map[UserId, Hand],
    board: Board, 
    pile: CardPile.DefaultPile,
    discardPile: CardPile.DiscardPile
)