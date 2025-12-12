package apps.ul2025app68

import cs214.webapp.server.StateMachine
import cs214.webapp.AppInfo
import cs214.webapp.UserId
import cs214.webapp.Action
import scala.util.Try
import cs214.webapp.AppWire
import scala.util.Random
import scala.caps.use
import cs214.webapp.IllegalMoveException
import cs214.webapp.Action.Render
import apps.ul2025app68.Card.*
import apps.ul2025app68.PhaseView.*
import scala.collection.mutable.Queue
import scala.languageFeature.experimental.macros

given MIN_NUMBER_OF_CARD_IN_HAND: Int = 5
val MAX_NUMBER_OF_CARD_IN_HAND: Int = 6

class Logic extends StateMachine[Event, State, View]:
    val appInfo: AppInfo = AppInfo(
        id = "ul2025app68",
        name = "Smile Life",
        description = "à modifier",
        year = 2025
    )

    override def wire: AppWire[Event, View] = Wire

    override def init(clients: Seq[UserId]): State = 
        val cardPiles: CardPiles = setPiles()
        val (hands, updatedCardPiles) = cardPiles.giveCardsTo(clients)
        val board: Board = clients.map((_, Vector.empty)).toMap
        val playerQueue: Queue[UserId] = Queue.from(clients) 
        State(
            hands = hands, 
            board = board, 
            cardPiles = updatedCardPiles,
            playerQueue = playerQueue
        )

    override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try:
        val State(hands, board, cardPiles, playerQueue) = state
        val cardsInHand: Vector[Card] = hands.get(userId).get  
        val nbOfCardsInHands: Int = cardsInHand.length
        if gameIsOver(state.cardPiles) then
            throw IllegalMoveException("Accept your defeat, the game is over")
        else if !isTurnOf(userId, playerQueue) then
            throw IllegalMoveException("Not your turn to play")
        else event match
            case Event.PlayCard(card: Card, against: UserId) =>
                val playerHand: PlayedHand = board.get(against).get
                if !cardsInHand.contains(card) || !card.canBePlaced(playerHand) then
                    throw IllegalMoveException("You can't play this card")

                else if nbOfCardsInHands == MIN_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalMoveException("You should draw a card first")

                else if nbOfCardsInHands != MAX_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                    )
                else 
                    val newBoard = card match
                        case MalusCard(malus) => malus match
                            case Malus.Disease => placeCard(card, against, board)
                            case Malus.Accident => placeCard(card, against, board)
                            case Malus.BurnOut => placeCard(card, against, board)
                            case Malus.Tax => removeCard(_.isInstanceOf[Money], against, board)
                            case Malus.Divorce => removeCard(_ == Marriage, against, board)
                            case Malus.Dismissal => removeCard(_.isInstanceOf[Profession], against, board)
                            case Malus.TerroristAttack => removeCard(_ == Child, against, board, all = true)
                            case Malus.RepeatYear => removeCard(_ == Study, against, board)
                        case _ =>
                            placeCard(card, userId, board)

                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardsInHand.patch(cardsInHand.indexOf(card), Nil, 1)
                    )
                    
                    Seq(
                        Render(state.copy(
                            hands = newHands,
                            board = newBoard,
                            playerQueue = toNextPlayer(state).playerQueue
                        ))
                    )

            case Event.Discard(card) =>
                if nbOfCardsInHands == MIN_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalMoveException("You haven't picked a card yet")
                else if nbOfCardsInHands == MAX_NUMBER_OF_CARD_IN_HAND then
                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardsInHand.patch(cardsInHand.indexOf(card), Nil, 1)
                    )
                    Seq(
                        Render(state.copy(
                            hands = newHands,
                            cardPiles = cardPiles.discard(card),
                            playerQueue = toNextPlayer(state).playerQueue
                        ))
                    )
                else 
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                    )

            case Event.PickCard(isDefaultPile) =>
                if nbOfCardsInHands == MIN_NUMBER_OF_CARD_IN_HAND then cardPiles.pickCard(isDefaultPile) match
                    case None =>
                        throw IllegalMoveException("You can't pick a card from an empty pile")

                    case Some((card, updatedCardPiles)) => 
                        Seq(
                            Render(state.copy(
                                hands = hands.updated(userId, cardsInHand.appended(card)),
                                cardPiles = updatedCardPiles
                            ))
                        )
                else if nbOfCardsInHands == MAX_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalMoveException("You can't pick a card, you already did")
                else 
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                    )

                        

    override def project(state: State)(userId: UserId): View = 
        val State(hands, board, cardPiles, playerQueue) = state

        if gameIsOver(state.cardPiles) then
            val winner = countSmiles(board).maxBy(_._2)._1
            View(VictoryView(List(winner)))

        else hands.get(userId) match
            case None =>
                throw IllegalArgumentException(f"The given userId \"${userId}\" is unknown")
            case Some(hand) => 
                val lastDiscard = if cardPiles.trashPile.isEmpty then None else Some(cardPiles.trashPile.head) // TEMPORARY eventuellement ajouter un type null?
                View(GameView(board, hand, lastDiscard, playerQueue.head, cardPiles.defaultPile.length))
        

/** 
  * Creates a new `DefaultPile` containing a randomly generated list of cards.
  *
  * @param rand
  *   The random number generator used to build the pile.
  *   Defaults to a newly created `Random` instance if not provided.
  *
  * @param size
  *   The number of cards to generate for the pile.
  *   Defaults to `30` (To change later).
  *
  * @return
  *   A `CardPile.DefaultPile` instance containing a list of randomly generated cards.
  */
def setPiles(rand: Random = Random, size: Int = 30): CardPiles =
    val studyCount = 28
    val moneyCount = 10 // per salary level
    val flirtCount = 20
    val marriageCount = 7
    val childCount = 10
    val petCount = 5
    val malusCount = 5 // per malus types
    val smallHouseCount = 2
    val mediumHouseCount = 2
    val bigHouseCount = 1
    val travelCount = 5

    val professions = List(
        // 0 studies
        Card.Profession(0, 1, None, "Stripper"),
        Card.Profession(0, 1, None, "Waiter"),
        Card.Profession(0, 1, None, "Writer"),
        Card.Profession(0, 1, None, "Medium"),
        Card.Profession(0, 3, None, "Guru"),
        Card.Profession(0, 2, None, "Pizza maker"),
        Card.Profession(0, 1, Some(List(Bonus.MalusProtection(Malus.Dismissal))), "Soldier"),
        Card.Profession(0, 4, Some(List(Bonus.MalusProtection(Malus.Tax),Bonus.MalusProtection(Malus.Dismissal))), "Bandit"),
        Card.Profession(0, 1, Some(List(Bonus.UnlimitedFlirt)), "Barman"),
        // 1 studies
        Card.Profession(1, 1, None, "Gardener"),
        Card.Profession(1, 1, Some(List(Bonus.MalusProtection(Malus.Dismissal))), "Policeman"),
        Card.Profession(1, 1, None, "Plumber"),
        Card.Profession(1, 2, Some(List(Bonus.MalusProtection(Malus.Accident))), "Mechanic"),
        // 2 studies
        Card.Profession(2, 2, Some(List(Bonus.MalusProtection(Malus.Dismissal))), "French Teacher"),
        Card.Profession(2, 2, Some(List(Bonus.MalusProtection(Malus.Dismissal))), "English Teacher"),
        Card.Profession(2, 2, Some(List(Bonus.MalusProtection(Malus.Dismissal))), "Math Teacher"),
        Card.Profession(2, 2, Some(List(Bonus.MalusProtection(Malus.Dismissal))), "History Teacher"),
        // 3 studies
        Card.Profession(3, 3, None, "Sales manager"),
        Card.Profession(3, 3, None, "Purchases manager"),
        Card.Profession(3, 2, None, "Journalist"),
        // 4 studies
        Card.Profession(4, 3, None, "Designer"),
        Card.Profession(4, 3, Some(List(Bonus.FreeHouse)), "Architect"),
        Card.Profession(4, 3, Some(List(Bonus.MalusProtection(Malus.Divorce))), "Lawyer"),
        // 5 studies
        Card.Profession(5, 3, Some(List(Bonus.MalusProtection(Malus.Disease))), "Pharmacist"),
        Card.Profession(5, 4, Some(List(Bonus.FreeTravel)), "Airline pilot"),
        // 6 studies
        Card.Profession(6, 4, Some(List( Bonus.MalusProtection(Malus.Disease), Bonus.UnlimitedStudy)), "Doctor"),
        Card.Profession(6, 2, None, "Researcher"),
        Card.Profession(6, 4, Some(List( Bonus.MalusProtection(Malus.Disease), Bonus.UnlimitedStudy)), "Surgeon"),
        Card.Profession(6, 4, None, "Astronaut")
    )

    val pile: List[Card] = List(
        List.fill(studyCount)(Card.Study),
        List.fill(moneyCount)(Card.Money(1)),
        List.fill(moneyCount)(Card.Money(2)),
        List.fill(moneyCount)(Card.Money(3)),
        List.fill(moneyCount)(Card.Money(4)),
        List.fill(marriageCount)(Card.Marriage),
        List.fill(childCount)(Card.Child),
        List.fill(petCount)(Card.Pet),
        List.fill(malusCount)(Card.MalusCard(Malus.Disease)),
        List.fill(malusCount)(Card.MalusCard(Malus.Accident)),
        List.fill(malusCount)(Card.MalusCard(Malus.BurnOut)),
        List.fill(malusCount)(Card.MalusCard(Malus.Tax)),
        List.fill(malusCount)(Card.MalusCard(Malus.Divorce)),
        List.fill(malusCount)(Card.MalusCard(Malus.Dismissal)),
        List.fill(malusCount)(Card.MalusCard(Malus.RepeatYear)),
        List.fill(1)(Card.MalusCard(Malus.TerroristAttack)),
        List.fill(smallHouseCount)(Card.House(2)),
        List.fill(mediumHouseCount)(Card.House(3)),
        List.fill(bigHouseCount)(Card.House(4)),
        List.fill(travelCount)(Card.Travel(3)),
        professions
    ).flatten

    val shuffled = scala.util.Random.shuffle(pile)

    CardPiles(shuffled, List.empty)


def placeCard(card: Card, userId: UserId, board: Board): Board =
    board.updated(
        userId, board.get(userId).get.appended(card)
    )

def removeCard(identifier: Card => Boolean, userId: UserId, board: Board, all: Boolean = false): Board =
    val playedHand = board.get(userId).get 
    if !playedHand.exists(identifier) then
        throw IllegalMoveException("This Malus can't be applied to this player")
    else if all then
        board.updated(
            userId, playedHand.filter(identifier)
        )
    else
        val index = playedHand.indexWhere(identifier)
        board.updated(
            userId, playedHand.patch(index, Nil, 1)
        )
    
    
    

/** 
  * Count the number of Smiles for each player in the game. 
  * ("Smiles" is the name for points, and the player with the most number of Smiles win)
  *
  * @param board
  *     A map that links each player with the cards they have played during the game.
  *
  * @return
  *   A Map associating each player with their number of points.
  */
def countSmiles(board: Board): Map[UserId, Int] =
    board.keySet.map(userId =>
        (userId, countSmiles(board, userId))
    ).toMap
    
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
def countSmiles(board: Board, userId: UserId): Int =
    def SmileValue(card: Card): Int =
        card match
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
    
    board(userId).map(SmileValue).sum
    
    /*val myCards: PlayedHand = board.getOrElse(userId, Vector.empty)
    //get playedHands of the required player
    val baseScore = 
        myCards.foldLeft(0) { //smile counter initialized at 0, for each played card associates a number of smiles
            case (acc, Card.Flirt) => acc + 1
            case (acc, Card.Child) => acc + 3
            case (acc, Card.Money) => acc
            case (acc, Card.Profession) => acc 
            case (acc, Card.Study) => acc 
            case (acc, Card.Pet) => acc + 2
            case (acc, Card.Special) => acc + 1
            case (acc, _) => acc
        }
    
    val malusAgainstMe =
        board.iterator
            .filter((otherId, _) => otherId != userId) //keeps only the cards played by other players
            .map { case (_, cards) => cards.count(_.isInstanceOf[Card.MalusCard]) } //counts the number of malus cards played by other players
            .sum //each malus card played by another player is a -1 malus to the score

    baseScore - malusAgainstMe //smiles obtained by placing cards minus smiles lost due to malus  
    */

// add documentation
def isTurnOf(userId: UserId, playerQueue: Queue[UserId]): Boolean =
    playerQueue.head == userId


//add documentation
def gameIsOver(cardpiles: CardPiles): Boolean =
    // change to Paramètre, met qq chose de plus précis que state -- done ?
    cardpiles.drawPileIsEmpty

def toNextPlayer(state : State): State =
    // regarde ce que j'ai modifier pour isTurnOf: c'est toujours au tour du premier joueur dans la queue de jouer
    // donc il faut que tu créer une nouvelle queue comme ça : toNextPlayer(Queue("1", "2", "3")) == Queue("2", "3", "1")
    val State(hands, board, cardPiles, playerQueue) = state
    if playerQueue.isEmpty then state
    else
        //use copies
        var queue = Queue.from(playerQueue)
        var b = board
        var piles = cardPiles
        var remaining = queue.size
        var done = false

        while !done && remaining > 0 do 
            queue = Queue.from(queue.tail :+ queue.head)
            val current = queue.head

            if hasSkipMalus(current,b) then
                val (newBoard,newCardPiles) = inflictSkipMalus(b,piles,current)
                b = newBoard
                piles = newCardPiles
                remaining -= 1
            else
                done = true

        State(hands,b,piles,queue)
    
    
    // faut faire en sorte que si un joueur a un malus alors on pass son tour
def hasSkipMalus(userId : UserId, board : Board): Boolean = 
    board.getOrElse(userId,Vector.empty).exists{
        case Card.MalusCard(Malus.Disease | Malus.Accident | Malus.BurnOut) => true
        case _ => false
    }

def inflictSkipMalus(board: Board, cardPiles: CardPiles, userId: UserId): (Board, CardPiles) =
    val playedCards = board.getOrElse(userId, Vector.empty)
    var removed = false
    val newHand =
        playedCards.flatMap {
            case c @ Card.MalusCard(m @ (Malus.Disease | Malus.Accident | Malus.BurnOut))
                if !removed =>
                    removed = true
                    None

            case c =>
                Some(c)
        }
    (board.updated(userId, newHand), cardPiles)


def handAfterPaying(playedHand: PlayedHand, amountToPay :Int): Option[PlayedHand] = 
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

                                    


                        
