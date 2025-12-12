package apps.ul2025app68

import cs214.webapp.UserId
import scala.collection.mutable.Queue

// Maluses
enum Malus:
    case Disease 
    case Accident
    case BurnOut
    case Tax
    case Divorce
    case Dismissal
    case TerroristAttack
    case RepeatYear // bad translation for "redoublement" should be changed

enum Bonus:
    case MalusProtection(malus: Malus)
    case FreeHouse
    case FreeTravel
    case UnlimitedFlirt
    case FlirtWhileMarried
    case UnlimitedStudy
    case StudyWhileWorking

enum Card:
    case Flirt
    case Marriage
    case Child
    case Study
    case Pet
    case MalusCard(malus: Malus)
    case House(price: Int)
    case Travel(price: Int)
    case Special(bonus: Bonus, name: String)
    case Money(amount: Int, used: Boolean = false)
    case Profession(studyRequired: Int, salary: Int, bonus: Option[Seq[Bonus]] = None, name: String)


    def canBePlaced(playedHand: PlayedHand): Boolean = this match
        case Money(amount,_) =>
            playedHand.collectFirst {case p: Profession => p} match
                case Some(profession) => profession.salary >= amount 
                case None => false
            
        case Profession(studyRequired, salary,_,_) =>
            val enoughStudy = playedHand.count(_ == Study) >= studyRequired
            val isJobLess = !playedHand.exists(_.isInstanceOf[Profession])
            isJobLess && enoughStudy

        case Study =>
            def withinLimit = (playedHand.count(_ == Study) <= 6) || playedHand.hasBonus(Bonus.UnlimitedStudy)
            def isNotWorking = !playedHand.exists(_.isInstanceOf[Profession]) || playedHand.hasBonus(Bonus.StudyWhileWorking)
            withinLimit && isNotWorking

        case Flirt => 
            def withinLimit = (playedHand.count(_ == Flirt) <= 5) || playedHand.hasBonus(Bonus.UnlimitedFlirt)
            def isNotMarried = !playedHand.exists(_ == Marriage) || playedHand.hasBonus(Bonus.FlirtWhileMarried)
            withinLimit && isNotMarried
            
        case Marriage => 
            playedHand.exists(_ == Flirt)

        case Child => 
            playedHand.exists(_ == Marriage)

        case MalusCard(malus) => 
            def hasNoProtection = !playedHand.hasBonus(Bonus.MalusProtection(malus))
            def canApply = malus match
                case Malus.Disease => true
                case Malus.Accident => true
                case Malus.BurnOut => playedHand.exists(_.isInstanceOf[Profession])
                case Malus.Tax => playedHand.exists(_.isInstanceOf[Money])
                case Malus.Divorce => playedHand.exists(_ == Marriage)
                case Malus.Dismissal => playedHand.exists(_.isInstanceOf[Profession])
                case Malus.TerroristAttack => playedHand.exists(_ == Child)
                case Malus.RepeatYear => playedHand.exists(_ == Study) && !playedHand.exists(_.isInstanceOf[Profession])
            
            hasNoProtection && canApply

        case Pet => true
            
        case Special(bonus, _) => true

        case House(price) => 
            def hasBonus = playedHand.hasBonus(Bonus.FreeHouse)
            def hasMoney = ???
            hasBonus || hasMoney

        case Travel(price) => 
            def hasBonus = playedHand.hasBonus(Bonus.FreeTravel)
            def hasMoney = ???
            hasBonus || hasMoney 

        
// object Card:
//     object Profession:
//         def unapply(p: Profession): Some[(Int, Int)] =
//             Some((p.studyRequired, p.salary))
        
//     object Money:
//         def unapply(m: Money): Some[Int] =
//             Some(m.amount)

//     object Special:
//         def unapply(s: Special): Some[Bonus] =
//             Some(s.bonus)


type Hand = Vector[Card]

type PlayedHand = // Or type Life ? Or type Deck ?
    Vector[Card]

extension (playedHand: PlayedHand)
    def hasBonus(bonus: Bonus): Boolean = playedHand.exists:
        case p: Card.Profession => p.bonus.isDefined && p.bonus.get.exists(_==bonus)
        case Card.Special(thatBonus, _) => thatBonus == bonus 
        case _ => false 

    // def count(card: Card): Int = playedHand.count(_ asInstanceOf card)


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
            print(defaultPile.head.toString)
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
    case PlayCard(card: Card, on: UserId)
    case PickCard(isDefaultPile: Boolean)
        // true -> DefaultPile
        // false -> DiscardPile

case class View(val phaseView: PhaseView)

enum PhaseView:
    case GameView(board: Board, hand: Hand, lastDiscard: Option[Card], turnOf: UserId, drawPileSize: Int)
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
    money: Seq[Card.Money],
    profession: Option[Card.Profession],
    study: Int,
    pet: Int,
    malus: Seq[Malus],
    special: Seq[Card.Special]
)

type FullBoard = Map[UserId, PlayerBoard]