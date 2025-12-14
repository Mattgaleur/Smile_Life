package apps.ul2025app68

import cs214.webapp.UserId
import scala.collection.immutable.Queue

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
    case DoubleStudy
    case DoubleMarriage

enum Card:
    case Flirt
    case Marriage
    case Child
    case Study
    case Pet
    case MalusCard(malus: Malus)
    case Travel(price: Int)
    case House(price: Int)
    case Special(bonus: Bonus, name: String)
    case Money(amount: Int, used: Boolean = false)
    case Profession(studyRequired: Int, salary: Int, bonus: Option[Seq[Bonus]] = None, name: String)
    
    def smileValue: Int =
        this match
            case Flirt => 1
            case Marriage => 3
            case Child => 2
            case Study => 1
            case Pet => 1
            case House(price) => price match
                case 2 => 1
                case 3 => 2
                case 4 => 3
            case Travel(price) => 1
            case Special(bonus, name) => 1
            case Money(amount, used) => 1
            case Profession(studyRequired, salary, bonus, name) => 2
            case _ => 0

        
type Hand = Vector[Card]

type PlayedHand = // Or type Life ? Or type Deck ?
    Vector[Card]                        

type Board = Map[UserId, PlayedHand]

type Pile = List[Card] // Maybe setting Pile as a mutable class would make things simpler

case class CardPiles(
    val defaultPile: Pile,
    val trashPile: Pile
)

enum Event:
    case Discard(card: Card)
    case PlayCard(card: Card, selectedUser: UserId)
    case PickCard(isDefaultPile: Boolean)
        // true -> DefaultPile
        // false -> DiscardPile
    case QuitJob
    case EndGame

case class View(val phaseView: PhaseView)

enum PhaseView:
    case GameView(board: Board, hand: Hand, lastDiscard: Option[Card], turnOf: UserId, drawPileSize: Int, log: Log)
    case VictoryView(winners: Seq[UserId], board: Board, log: Log)


case class State(
    hands: Map[UserId, Hand],
    board: Board, 
    cardPiles: CardPiles,
    playerQueue: Queue[UserId],
    log: Log
)

type Log = List[String]        

case class PlayerBoard(
    flirt: Int,
    child: Int,
    money: Seq[Card.Money],
    profession: Option[Card.Profession],
    study: Int,
    pet: Int,
    malus: Seq[Malus],
    special: Seq[Card.Special],
    houses: Seq[Card.House],
    travels: Int,
    marriage: Int,
    smiles: Int
)

type FullBoard = Map[UserId, PlayerBoard]