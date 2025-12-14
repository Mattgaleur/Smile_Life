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

    def canBePlaced(playedHand: PlayedHand): Boolean = this match
        case Money(amount,_) =>
            playedHand.collectFirst {case p: Profession => p} match
                case Some(profession) => profession.salary >= amount 
                case None => false
            
        case Profession(studyRequired, salary,_,_) =>
            val studyMultiplier = if playedHand.hasBonus(Bonus.DoubleStudy) then 2 else 1
            val studies = playedHand.count(_ == Study) * studyMultiplier
            val enoughStudy =  studies >= studyRequired
            val isJobLess = !playedHand.exists(_.isInstanceOf[Profession])
            isJobLess && enoughStudy

        case Study =>
            println(f"Study count: ${playedHand.count(_ == Study)}")
            println(f"playedHand: $playedHand")
            def withinLimit = (playedHand.count(_ == Study) < 6) || playedHand.hasBonus(Bonus.UnlimitedStudy)
            def isNotWorking = !playedHand.exists(_.isInstanceOf[Profession]) || playedHand.hasBonus(Bonus.StudyWhileWorking)
            withinLimit && isNotWorking

        case Flirt => 
            val flirts = playedHand.count(_ == Flirt)
            def withinLimit = (flirts < 5) || playedHand.hasBonus(Bonus.UnlimitedFlirt)
            def isNotMarried = !playedHand.exists(_ == Marriage) || playedHand.hasBonus(Bonus.FlirtWhileMarried)
            withinLimit && isNotMarried
            
        case Marriage => 
            if playedHand.hasBonus(Bonus.DoubleMarriage) then 
                playedHand.count(_ == Marriage) < 2
                else
                    playedHand.exists(_ == Flirt)

        case Child => 
            playedHand.exists(_ == Marriage)

        case MalusCard(malus) => 
            def hasNoProtection = !playedHand.hasBonus(Bonus.MalusProtection(malus))
            def canApply = malus match
                case Malus.Disease => true
                case Malus.Accident => true
                case Malus.BurnOut => playedHand.exists(_.isInstanceOf[Profession])
                case Malus.Tax => playedHand.exists:
                    case m: Money => !m.used
                    case _ => false
                case Malus.Divorce => playedHand.exists(_ == Marriage)
                case Malus.Dismissal => playedHand.exists(_.isInstanceOf[Profession])
                case Malus.TerroristAttack => playedHand.exists(_ == Child)
                case Malus.RepeatYear => playedHand.exists(_ == Study) && !playedHand.exists(_.isInstanceOf[Profession])
            
            hasNoProtection && canApply

        case Pet => true
            
        case Special(bonus, _) => true

        case House(price) => 
            def hasBonus = playedHand.hasBonus(Bonus.FreeHouse)
            def hasMoney = playedHand.handAfterPaying(price).isDefined
            hasBonus || hasMoney

        case Travel(price) => 
            def hasBonus = playedHand.hasBonus(Bonus.FreeTravel)
            def hasMoney = playedHand.handAfterPaying(price).isDefined
            hasBonus || hasMoney 
    
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

    def handAfterPaying(amountToPay: Int): Option[PlayedHand] = 
        if amountToPay <= 0 then
            Some(playedHand)
        else
            val availableMoney: List[(Int,Int)] = // (index in hand, money)
                playedHand.zipWithIndex.collect{
                    case (m @ Card.Money(amount, used), idx) if !used => // filter cards that are Money and not used
                        (idx, amount)
                }.toList
            
            if availableMoney.isEmpty then None //if no money placed, no way to pay
            else
                val totalAvailable = availableMoney.map(_._2).sum // (idx,amount) so we take _.2 and sum to have all available money
                if totalAvailable < amountToPay then // if not enough money to pay, no way to pay
                    None
                else // if enough money to pay
                    var bestCombo: Option[(Int,List[(Int,Int)])] = None // (totalAvailable,List[(idx,amount)]), the bestCombo is None at first
                    for 
                        k <- 1 to availableMoney.length
                        combo <- availableMoney.combinations(k) //all combinations of the money we have possible
                    do
                        val sum = combo.map(_._2).sum
                        if sum >= amountToPay then
                            val overpay = sum - amountToPay
                            bestCombo match
                                case None => //initialized at None, updated in first iteration
                                    bestCombo = Some((sum,combo.toList))
                                case Some((bestSum,bestList)) => // compare with previous best way to pay to determine which is better
                                    val bestOverpay = bestSum - amountToPay
                                    if overpay < bestOverpay || (overpay == bestOverpay && combo.size < bestList.size) then 
                                        // if pay less payed or if same payed but higher cards (keep smallest for more possibilities)
                                        bestCombo = Some((sum,combo.toList))

                    bestCombo match
                        case None => None
                        case Some((_,chosen)) => 
                            val indicesToUse: Set[Int]= chosen.map(_._1).toSet // Set of indexes of Money cards of the best way to pay
                            val newHand: PlayedHand = 
                                playedHand.zipWithIndex.map {
                                    case (m @ Card.Money(amount,used), idx) if indicesToUse.contains(idx) => 
                                        m.copy(used = true) // if Money is used to pay here, copy but used = True now
                                    case (card, _) => card  // cards that are not money stay the same
                                }
                            Some(newHand)

                                        


                        

type Board = Map[UserId, PlayedHand]

extension (board: Board)
    /** 
     * Count the number of Smiles for a specific player in the game. 
     * ("Smiles" is the name for points, and the player with the most number of Smiles win)
     *
     * @param board
     *     A map that links each player with the cards they have played during the game.
     *
     * @param userId
     *     The user for which we want to calculate the number of Smiles.
     * 
     * @return
     *   A Map associating each player with their number of points.
    */
    def countSmiles(userId: UserId): Int =
        board(userId).map(card => card.smileValue).sum

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
    case PlayCard(card: Card, selectedUser: UserId)
    case PickCard(isDefaultPile: Boolean)
        // true -> DefaultPile
        // false -> DiscardPile
    case QuitJob
    case EndGame

case class View(val phaseView: PhaseView)

enum PhaseView:
    case GameView(board: Board, hand: Hand, lastDiscard: Option[Card], turnOf: UserId, drawPileSize: Int, log: Log)
    case VictoryView(winners: Seq[UserId])


case class State(
    hands: Map[UserId, Hand],
    board: Board, 
    cardPiles: CardPiles,
    playerQueue: Queue[UserId],
    log: Log
)

type Log = List[String]

extension (log: Log)
    def write(userId: UserId)(event: Event): Log =
        event match
            case Event.Discard(card) => 
                f"$userId discarded a ${card.productPrefix} card" :: log

            case Event.PickCard(isDefaultPile) =>
                if isDefaultPile then
                    f"$userId drew a card from the Pile" :: log
                else 
                    f"$userId drew a card from the Trash Pile" :: log

            case Event.QuitJob =>
                f"$userId decided to quit his job" :: log

            case Event.EndGame =>
                f"$userId is tired of playing, he ended the game" :: log

            case Event.PlayCard(card, selectedUser) => card match
                case Card.MalusCard(malus) => 
                    f"$userId used $malus on $selectedUser" :: log 
                case Card.Travel(price) =>
                    f"$userId planned a Travel for $price" :: log
                case Card.House(price) =>
                    f"$userId bought a House for $price" :: log
                case Card.Special(bonus, name) =>
                    f"$userId played the $name" :: log
                case Card.Money(amount, used) =>
                    f"$userId earned $amount Money" :: log
                case Card.Profession(studyRequired, salary, bonus, name) =>
                    f"$userId became a $name" :: log
                case Card.Flirt =>
                    f"$userId decided to Flirt" :: log
                case Card.Marriage =>
                    f"$userId married a random person, congrats!" :: log
                case Card.Child =>
                    f"$userId had a child with his partner" :: log
                case Card.Study =>
                    f"$userId spent one year to Study" :: log
                case Card.Pet =>
                    f"$userId bought a pet" :: log
        

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