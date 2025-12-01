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
        val playerHand: PlayedHand = board.get(userId).get
        if gameIsOver(state) then
            throw IllegalMoveException("Accept your defeat, the game is over")
        else if !isTurnOf(userId, playerQueue) then
            throw IllegalMoveException("Not your turn to play")
        else event match
            case Event.PlayCard(card: Card) =>
                if !cardsInHand.contains(card) || !card.canBePlaced(playerHand) then
                    throw IllegalMoveException("You can't play this card")

                else if nbOfCardsInHands == MIN_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalMoveException("You should draw a card first")

                else if nbOfCardsInHands == MAX_NUMBER_OF_CARD_IN_HAND then
                    val newBoard: Board = board.updated(
                        userId, board.get(userId).get.appended(card)
                    )
                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardsInHand.patch(cardsInHand.indexOf(card), Nil, 1)
                    )
                    Seq(
                        Render(state.copy(
                            hands = newHands,
                            board = newBoard,
                            playerQueue = toNextPlayer(playerQueue)
                        ))
                    )
                else 
                    throw IllegalStateException(
                        f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
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
                            playerQueue = toNextPlayer(playerQueue)
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

        if gameIsOver(state) then
            val winner = countSmiles(board).maxBy(_._2)._1
            View(VictoryView(List(winner)))

        else hands.get(userId) match
            case None =>
                throw IllegalArgumentException(f"The given userId \"${userId}\" is unknown")
            case Some(hand) => 
                val lastDiscard = if cardPiles.trashPile.isEmpty then Special else cardPiles.trashPile.head // TEMPORARY eventuellement ajouter un type null?
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
    ???
    // val allCards = Card.values // all possibles card types from the Card enum
    // val defaultPile: Pile =  
    //     List.fill(size) { //fill a list of size = input parameter "size"
    //         val i = rand.between(0, allCards.length) 
    //         //pick a random index between 0 and the number of different types of Card
    //         allCards(i)
    //         //we fill the pile with the random card type selected, ex: 0=>Flirt, 1=>Child, etc...
    //     }
    // CardPiles(defaultPile, List.empty) //return a CardPile with our random draw pile and an empty discard pile


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
            case Child => 2
            case Money => 1
            case Profession => 2
            case Study => 1
            case Pet => 1
            case Malus => -1
            case Special => +1
        
    board(userId).map(SmileValue).sum
    
    val myCards: PlayedHand = board.getOrElse(userId, Vector.empty)
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
            .map { case (_, cards) => cards.count(_ == Card.Malus) } //counts the number of malus cards played by other players
            .sum //each malus card played by another player is a -1 malus to the score

    baseScore - malusAgainstMe //smiles obtained by placing cards minus smiles lost due to malus  


// add documentation
def isTurnOf(userId: UserId, playerQueue: Queue[UserId]): Boolean =
    playerQueue.head == userId


//add documentation
def gameIsOver(state: State): Boolean =
    // change to Paramètre, met qq chose de plus précis que state
    val cardPiles = state.cardPiles //get the draw and discard piles from the state
    cardPiles.drawPileIsEmpty

def toNextPlayer(playerQueue: Queue[UserId]): Queue[UserId] =
    val next = playerQueue.dequeue()
    val newQueue = Queue.from(playerQueue)  // copy remaining players
    newQueue.enqueue(next)
    newQueue
    