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
import scala.languageFeature.experimental.macros
import scala.collection.immutable.Queue

val DEFAULT_CARD_IN_HAND: Int = 5
val CARDS_IN_HAND_AFTER_DRAW: Int = 6

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
        else if !state.isTurnOf(userId) then
            throw IllegalMoveException("Not your turn to play")
        else event match
            case Event.PlayCard(card: Card, selectedUser: UserId) =>
                val playerHand: PlayedHand = board(selectedUser)
                if !cardsInHand.contains(card) || !card.canBePlaced(playerHand) then
                    throw IllegalMoveException("You can't play this card")

                else if nbOfCardsInHands == DEFAULT_CARD_IN_HAND then
                    throw IllegalMoveException("You should draw a card first")

                else if nbOfCardsInHands != CARDS_IN_HAND_AFTER_DRAW then
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                    )
                else 
                    val newBoard = card match
                        case MalusCard(malus) => malus match
                            case Malus.Disease => board.placeCard(card, selectedUser)
                            case Malus.Accident => board.placeCard(card, selectedUser)
                            case Malus.BurnOut => board.placeCard(card, selectedUser)
                            case Malus.Tax => board.removeCard(selectedUser):
                                case m: Money => !m.used
                                case _ => false
                            case Malus.Divorce => board.removeCard(selectedUser)(_ == Marriage)
                            case Malus.Dismissal => board.removeCard(selectedUser)(_.isInstanceOf[Profession])
                            case Malus.TerroristAttack => board.removeCard(selectedUser, all = true)(_ == Child)
                            case Malus.RepeatYear => board.removeCard(selectedUser)(_ == Study)
                        case Travel(price) =>
                            val newPlayedHandOpt = playerHand.handAfterPaying(price)
                            if userId != selectedUser then
                                throw IllegalMoveException("You should play this card on yourself")
                            else if newPlayedHandOpt.isEmpty && !playerHand.hasBonus(Bonus.FreeTravel) then
                                throw IllegalMoveException("You don't have enough money to Travel")
                            else
                                board.updated(userId, newPlayedHandOpt.get).placeCard(card, userId)
                        case House(price) =>
                            val newPlayedHandOpt = playerHand.handAfterPaying(price)
                            if userId != selectedUser then
                                throw IllegalMoveException("You should play this card on yourself")
                            else if newPlayedHandOpt.isEmpty && !playerHand.hasBonus(Bonus.FreeHouse) then
                                throw IllegalMoveException("You don't have enough money by this House")
                            else
                                board.updated(userId, newPlayedHandOpt.get).placeCard(card, userId)
                        case _ =>
                            if userId != selectedUser then
                                throw IllegalMoveException("You should play this card on yourself")
                            else 
                                board.placeCard(card, userId)

                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardsInHand.patch(cardsInHand.indexOf(card), Nil, 1)
                    )
                    
                    Seq(
                        Render(
                            state.copy(
                                hands = newHands,
                                board = newBoard,
                                log = log.write(userId)(event)
                            ).toNextPlayer
                        )
                    )

            case Event.Discard(card) =>
                if nbOfCardsInHands == DEFAULT_CARD_IN_HAND then
                    throw IllegalMoveException("You haven't picked a card yet")
                else if nbOfCardsInHands == CARDS_IN_HAND_AFTER_DRAW then
                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardsInHand.patch(cardsInHand.indexOf(card), Nil, 1)
                    )
                    Seq(
                        Render(
                            state.copy(
                                hands = newHands,
                                cardPiles = cardPiles.discard(card),
                                log = log.write(userId)(event)
                            ).toNextPlayer
                        )
                    )
                else 
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                    )

            case Event.PickCard(isDefaultPile) =>
                if nbOfCardsInHands == DEFAULT_CARD_IN_HAND then cardPiles.pickCard(isDefaultPile) match
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
                else if nbOfCardsInHands == CARDS_IN_HAND_AFTER_DRAW then
                    throw IllegalMoveException("You can't pick a card, you already did")
                else 
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                    )
            case Event.QuitJob =>
                val playerHand: PlayedHand = board.get(userId).get
                if !playerHand.exists(_.isInstanceOf[Profession]) then
                    throw IllegalMoveException("You can't quit your job, you don't have one")
                else if nbOfCardsInHands == CARDS_IN_HAND_AFTER_DRAW then
                    throw IllegalMoveException("You can't pick a card, you already did")
                else if nbOfCardsInHands != DEFAULT_CARD_IN_HAND then
                    throw IllegalMoveException(f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal")
                else 
                    val newBoard = board.removeCard(userId)(_.isInstanceOf[Profession])
                    Seq(
                        Render(
                            state.copy(
                                board = newBoard,
                                log = log.write(userId)(event)
                            ).toNextPlayer
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
            val maxSmiles = board.countSmilesMap.values.max          
            val winners = board.countSmilesMap.filter(_._2 == maxSmiles).keys.toSeq       
            View(VictoryView(winners, board, log))

        else hands.get(userId) match
            case None =>
                throw IllegalArgumentException(f"The given userId \"${userId}\" is unknown")
            case Some(hand) => 
                val lastDiscard = if cardPiles.trashPile.isEmpty then None else Some(cardPiles.trashPile.head) 
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
def setPiles(rand: Random = Random): CardPiles =
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
        Card.Profession(6, 4, Some(List( Bonus.MalusProtection(Malus.Disease), Bonus.StudyWhileWorking)), "Doctor"),
        Card.Profession(6, 2, None, "Researcher"),
        Card.Profession(6, 4, Some(List( Bonus.MalusProtection(Malus.Disease), Bonus.StudyWhileWorking)), "Surgeon"),
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
        professions,
        specials
    ).flatten

    val shuffled = rand.shuffle(pile)
    CardPiles(shuffled, List.empty)        




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
    cardpiles.defaultPile.isEmpty


                                    


                        
