package apps.ul2025app68

import cs214.webapp.UserId
import scala.collection.mutable.Queue

enum Card:
    case Flirt
    case Child
    case Money(amount: Int)
    case Profession(studyRequired: Int, salary: Int)
    case Study
    case Pet
    case Malus
    case Special

    def canBePlaced(playedHand: PlayedHand): Boolean =
        this match
            case Money(amount) =>
                playedHand.collectFirst {case p: Profession => p } match
                    case Some(profession) => profession.salary >= amount 
                    case None => false
                
            case Profession(studyRequired, salary) =>
                val enoughStudy = playedHand.count(_ == Study) >= studyRequired
                val isJobLess = !playedHand.exists: // Maybe do an extension for PlayedHand
                    case Profession => true
                    case _ => false 
                isJobLess && enoughStudy

            case Study =>
                !playedHand.exists: 
                    case Profession => true
                    case _ => false

            // case Flirt => 
            // case Child =>
            // case Pet =>
            // case Malus =>
            // case Special =>
            case _ => true
        


type Hand = Vector[Card]

type PlayedHand = // Or type Life ? Or type Deck ?
    Vector[Card]

type Board = Map[UserId, PlayedHand]

type Pile = List[Card] // Maybe setting Pile as a mutable class would make things simpler

case class CardPiles(
    val defaultPile: Pile,
    val trashPile: Pile
) {
    def discard(card: Card): CardPiles =
        this.copy(
            trashPile = card :: trashPile
        )

    def pickCard(fromDefaultPile: Boolean): Option[(Card, CardPiles)] =
        if fromDefaultPile && defaultPile.nonEmpty then
            Some(
                defaultPile.head, 
                this.copy(defaultPile = defaultPile.tail)
            ) 
        else if !fromDefaultPile && trashPile.nonEmpty then
            Some(
                trashPile.head, 
                this.copy(trashPile = trashPile.tail)
            ) 
        else 
            None
    
    def giveCardsTo(clients: Seq[UserId])(using nbOfCards: Int): (Map[UserId, Hand], CardPiles) =
        val hands: Map[UserId, Hand] =
            clients
                .zip(defaultPile.grouped(nbOfCards))   // gives (userId, cards)
                .map { case (id, cards) => id -> cards.toVector }
                .toMap

        val piles: CardPiles = CardPiles(defaultPile.drop(clients.length * nbOfCards), List.empty)
        (hands, piles)

    override def equals(that: Any): Boolean = 
        that match
            case CardPiles(defaultPile, trashPile) => 
                this.defaultPile == defaultPile && this.trashPile == trashPile
            case _ => 
                false
    
    def drawPileIsEmpty: Boolean = 
        defaultPile.isEmpty
            
        
}

enum Event:
    case Discard(card: Card)
    case PlayCard(card: Card)
    case PickCard(isDefaultPile: Boolean)
        // true -> DefaultPile
        // false -> DiscardPile

case class View(val phaseView: PhaseView)

enum PhaseView:
    case GameView(board: Board, hand: Hand, lastDiscard: Card, turnOf: UserId, drawPileSize: Int)
    case VictoryView(winners: List[UserId])


case class State(
    hands: Map[UserId, Hand],
    board: Board, 
    cardPiles: CardPiles,
    playerQueue: Queue[UserId]
)

case class PlayerBoard(
    flirt: Int,
    child: Int,
    money: Seq[Card],
    profession: Option[Card],
    study: Int,
    pet: Int,
    malus: Seq[Card],
    special: Seq[Card]
)

type FullBoard = Map[UserId, PlayerBoard]