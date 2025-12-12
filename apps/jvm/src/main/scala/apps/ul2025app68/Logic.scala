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
        description = "Jeu de carte humoristique ou l'on construit sa vie",
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
            playerQueue = playerQueue,
            log = List("The Game Started")
        )

    override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try:
        val State(hands, board, cardPiles, playerQueue, log) = state
        val cardsInHand: Vector[Card] = hands.get(userId).get  
        val nbOfCardsInHands: Int = cardsInHand.length
        if gameIsOver(state.cardPiles) then
            throw IllegalMoveException("Accept your defeat, the game is over")
        else if !isTurnOf(userId, playerQueue) then
            throw IllegalMoveException("Not your turn to play")
        else event match
            case Event.PlayCard(card: Card, selectedUser: UserId) =>
                val playerHand: PlayedHand = board(selectedUser)
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
                            case Malus.Disease => placeCard(card, selectedUser, board)
                            case Malus.Accident => placeCard(card, selectedUser, board)
                            case Malus.BurnOut => placeCard(card, selectedUser, board)
                            case Malus.Tax => removeCard(_.isInstanceOf[Money], selectedUser, board)
                            case Malus.Divorce => removeCard(_ == Marriage, selectedUser, board)
                            case Malus.Dismissal => removeCard(_.isInstanceOf[Profession], selectedUser, board)
                            case Malus.TerroristAttack => removeCard(_ == Child, selectedUser, board, all = true)
                            case Malus.RepeatYear => removeCard(_ == Study, selectedUser, board)
                        case expenses: Expenses =>
                            val newPlayedHandOpt = playerHand.handAfterPaying(expenses.price)
                            if userId != selectedUser then
                                throw IllegalMoveException("You should play this card on yourself")
                            else if newPlayedHandOpt.isEmpty then
                                throw IllegalMoveException("You don't have enough money")
                            else
                                board.updated(userId, newPlayedHandOpt.get)

                        case _ =>
                            if userId != selectedUser then
                                throw IllegalMoveException("You should play this card on yourself")
                            else 
                                placeCard(card, userId, board)

                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardsInHand.patch(cardsInHand.indexOf(card), Nil, 1)
                    )
                    
                    Seq(
                        Render(
                            toNextPlayer(
                                state.copy(
                                    hands = newHands,
                                    board = newBoard,
                                    log = log.write(userId)(event)
                                )
                            )
                        )
                    )

            case Event.Discard(card) =>
                if nbOfCardsInHands == MIN_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalMoveException("You haven't picked a card yet")
                else if nbOfCardsInHands == MAX_NUMBER_OF_CARD_IN_HAND then
                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardsInHand.patch(cardsInHand.indexOf(card), Nil, 1)
                    )
                    Seq(
                        Render(
                            state.copy(
                                hands = newHands,
                                cardPiles = cardPiles.discard(card),
                                playerQueue = toNextPlayer(state).playerQueue,
                                log = log.write(userId)(event)
                            )
                        )
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
                            Render(
                                state.copy(
                                    hands = hands.updated(userId, cardsInHand.appended(card)),
                                    cardPiles = updatedCardPiles,
                                    log = log.write(userId)(event)
                                )
                            )
                        )
                else if nbOfCardsInHands == MAX_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalMoveException("You can't pick a card, you already did")
                else 
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                    )
            case Event.QuitJob =>
                val playerHand: PlayedHand = board.get(userId).get
                if !playerHand.exists(_.isInstanceOf[Profession]) then
                    throw IllegalMoveException("You can't quit your job, you don't have one")
                else 
                    val newBoard = removeCard(_.isInstanceOf[Profession], userId, board)
                    Seq(
                        Render(
                            toNextPlayer(
                                state.copy(
                                    board = newBoard,
                                    log = log.write(userId)(event)
                                )
                            )
                        )
                    )
            case Event.EndGame =>
                Seq(
                    Render(
                        state.copy(
                            cardPiles = CardPiles(List.empty, List.empty),
                            log = log.write(userId)(event)
                        )
                    )
                )

                        

    override def project(state: State)(userId: UserId): View = 
        val State(hands, board, cardPiles, playerQueue, log) = state

        if gameIsOver(state.cardPiles) then
            val maxSmiles = countSmilesMap(board).values.max          
            val winners = countSmilesMap(board).filter(_._2 == maxSmiles).keys.toSeq       
            View(VictoryView(winners))

        else hands.get(userId) match
            case None =>
                throw IllegalArgumentException(f"The given userId \"${userId}\" is unknown")
            case Some(hand) => 
                val lastDiscard = if cardPiles.trashPile.isEmpty then None else Some(cardPiles.trashPile.head) // TEMPORARY eventuellement ajouter un type null?
                View(GameView(board, hand, lastDiscard, playerQueue.head, cardPiles.defaultPile.length, log))
        

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

     val specials = List(
        Card.Special(Bonus.DoubleStudy, "Genius"),
        Card.Special(Bonus.DoubleMarriage, "Polygamy"),
        Card.Special(Bonus.MalusProtection(Malus.Tax), "Black Market"),
        Card.Special(Bonus.MalusProtection(Malus.BurnOut), "Hard Worker"),
        Card.Special(Bonus.UnlimitedStudy, "PHD"),
        Card.Special(Bonus.MalusProtection(Malus.Accident), "Car Insurance")
    )

    val pile: List[Card] = List(
        List.fill(studyCount)(Card.Study),
        List.fill(flirtCount)(Card.Flirt),
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


/** Places a card onto a player's played hand (their board).
  *
  * The card is appended at the end of the player's vector of played cards.
  *
  * @param card
  *   The card to place.
  * @param userId
  *   The player receiving the card (could be self or another player, depending on rules).
  * @param board
  *   The current board state.
  * @return
  *   A new board where `card` has been appended to `userId`'s played hand.
  */
def placeCard(card: Card, userId: UserId, board: Board): Board =
    board.updated(
        userId, board.get(userId).get.appended(card)
    )


/** Removes a card from a player's played hand, based on a predicate.
  *
  * Used to apply maluses that destroy/remove some existing card(s) from the target player.
  *
  * Behavior:
  *   - If the player does not have any card satisfying `identifier`, the move is illegal.
  *   - If `all = false`, removes exactly one matching card (the first match).
  *   - If `all = true`, removes all matching cards.
  *
  * @param identifier
  *   Predicate that returns true for the cards we want to remove.
  * @param userId
  *   The target player.
  * @param board
  *   The current board.
  * @param all
  *   If true, remove all matching cards; otherwise remove only one.
  * @throws IllegalMoveException
  *   If no matching card exists in the target player's played hand.
  * @return
  *   A new board where the matching card(s) have been removed from `userId`'s played hand.
  */
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
def countSmilesMap(board: Board): Map[UserId, Int] =
    board.keySet.map(userId =>
        (userId, board.countSmiles(userId))
    ).toMap
    


/** Checks whether it is currently `userId`'s turn to play.
  *
  * The rule is simple: the player who is at the head of `playerQueue`
  * is the only one allowed to perform an action.
  *
  * @param userId
  *   The player who is trying to act.
  * @param playerQueue
  *   The queue representing turn order. The head is the current player.
  * @return
  *   True iff `userId` is the head of the queue.
  */
def isTurnOf(userId: UserId, playerQueue: Queue[UserId]): Boolean =
    playerQueue.head == userId


/** Determines whether the game is over.
  *
  * In this game, the end condition is currently defined as:
  * the draw pile (default pile) is empty.
  *
  * @param cardpiles
  *   The current piles (draw pile + trash pile).
  * @return
  *   True iff there are no cards left to draw.
  */
def gameIsOver(cardpiles: CardPiles): Boolean =
    // change to Paramètre, met qq chose de plus précis que state -- done ?
    cardpiles.drawPileIsEmpty


/** Advances the game to the next player, while applying "skip turn" maluses.
  *
  * This method rotates the `playerQueue` (cyclic order), and if the next
  * player is affected by a skip-malus (Disease, Accident, BurnOut),
  * then that player is skipped:
  *   - the queue is rotated again
  *   - the malus is consumed (destroyed from the player's board)
  *
  * The method keeps skipping until it finds a player that does NOT have
  * a skip-malus, or until it has checked everyone once (to avoid infinite loops).
  *
  * @param state
  *   The current state (hands, board, piles, queue, log).
  * @return
  *   A new state where:
  *     - `playerQueue` has been rotated to the next valid player
  *     - skipped players had one skip-malus removed from their board
  *     - all other fields remain the same except the updated board/piles.
  */
def toNextPlayer(state : State): State =
    // regarde ce que j'ai modifier pour isTurnOf: c'est toujours au tour du premier joueur dans la queue de jouer
    // donc il faut que tu créer une nouvelle queue comme ça : toNextPlayer(Queue("1", "2", "3")) == Queue("2", "3", "1")
    val State(hands, board, cardPiles, playerQueue, log) = state
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

        State(hands, b, piles, queue, log)
    
    

/** Checks whether a given player currently has a "skip turn" malus.
  *
  * A skip-malus is one of:
  *   - Disease
  *   - Accident
  *   - BurnOut
  *
  * If at least one of those malus cards exists in the player's played hand,
  * then the player should lose their next turn.
  *
  * @param userId
  *   The player we check.
  * @param board
  *   The game board mapping each player to their played cards.
  * @return
  *   True iff the player's played hand contains a skip-malus.
  */
def hasSkipMalus(userId : UserId, board : Board): Boolean = 
    board.getOrElse(userId,Vector.empty).exists{
        case Card.MalusCard(Malus.Disease | Malus.Accident | Malus.BurnOut) => true
        case _ => false
    }


/** Consumes (destroys) exactly one skip-malus from a player's played hand.
  *
  * This is used when a player is skipped: the malus should not remain forever,
  * so we remove only the first occurrence of a skip-malus from their board.
  *
  * Important: the malus is NOT added to the trash pile (it is destroyed),
  * because players should not be able to pick it up again.
  *
  * @param board
  *   The board mapping each player to their played cards.
  * @param cardPiles
  *   The piles (kept unchanged here, because skip maluses are destroyed).
  * @param userId
  *   The skipped player whose malus we consume.
  * @return
  *   A pair (newBoard, newCardPiles) where:
  *     - newBoard is the board with one skip-malus removed from `userId`
  *     - newCardPiles is unchanged
  */
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


                                    


                        
