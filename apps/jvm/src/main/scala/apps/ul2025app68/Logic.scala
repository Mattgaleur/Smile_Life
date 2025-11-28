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
        State(
            hands = hands, 
            board = board, 
            cardPiles = updatedCardPiles
        )

    override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try:
        val State(hands, board, cardPiles) = state
        val cardsInHand: Vector[Card] = hands.get(userId).get  
        val nbOfCardsInHands: Int = cardsInHand.length
        if gameIsOver(state) then
            throw IllegalMoveException("Accept your defeat, the game is over")
        else if isTurnOf(userId, ???) then
            event match
                case Event.PlayCard(card) =>
                    if cardsInHand.contains(card) then
                        val newBoard: Board = board.updated(
                            userId, board.get(userId).get.appended(card)
                        )
                        val newHands: Map[UserId, Hand] = hands.updated(
                            userId, cardsInHand.drop(cardsInHand.indexOf(card))
                        )
                        Seq(
                            Render(state.copy(
                                hands = newHands,
                                board = newBoard
                            ))
                        )
                    else
                        throw IllegalMoveException("You can't play a card you don't have")

                case Event.Discard(card) =>
                    if nbOfCardsInHands == MIN_NUMBER_OF_CARD_IN_HAND then
                        throw IllegalMoveException("You haven't picked a card yet")
                    else if nbOfCardsInHands == MAX_NUMBER_OF_CARD_IN_HAND then
                        val newHands: Map[UserId, Hand] = hands.updated(
                            userId, cardsInHand.drop(cardsInHand.indexOf(card))
                        )
                        Seq(
                            Render(state.copy(
                                hands = newHands,
                                cardPiles = cardPiles.discard(card)
                            ))
                        )
                    else 
                        throw IllegalStateException(
                            f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                        )

                case Event.PickCard(isDefaultPile) =>
                    if nbOfCardsInHands == MIN_NUMBER_OF_CARD_IN_HAND then
                        cardPiles.pickCard(isDefaultPile) match
                            case Some((card, updatedCardPiles)) => 
                                Seq(
                                    Render(state.copy(
                                        hands = hands.updated(userId, cardsInHand.appended(card)),
                                        cardPiles = updatedCardPiles
                                    ))
                                )
                            case None =>
                                throw IllegalMoveException("You can't pick a card from an empty pile")
                    else if nbOfCardsInHands == MAX_NUMBER_OF_CARD_IN_HAND then
                        throw IllegalMoveException("You can't pick a card, you already did")
                    else 
                        throw IllegalStateException(
                            f"Impossible situation happened: you have ${nbOfCardsInHands} cards, which is Illegal"
                        )
        else
            throw IllegalMoveException("Not your turn to play")

                
        

    override def project(state: State)(userId: UserId): View = 
        val State(hands, board, cardPiles) = state
        if gameIsOver(state) then
            ???
        else
            hands.get(userId) match
                case Some(hand) => 
                    View(
                        board = board,
                        hand = hand    
                    )
                case None =>
                    throw IllegalArgumentException(f"The given userId \"${userId}\" is unknown")
        

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
    val allCards = Card.values
    val defaultPile: Pile =  // Pour toi Coco ;)
        List.fill(size) {
            val i = rand.between(0, allCards.length) 
            //pour chaque value de card possible (Flirt, Child, Money,..) on associe un Int
            allCards(i)
            //on fill une pile (List[Card]) avec une card du type associé au Int(index i) ex: 0: Flirt, 1: Child etc..
        }
    CardPiles(defaultPile, List.empty)


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
    // Pour toi Coco ;)
    // mon idée pour la version 1 qui sera très simplifier:
        // Flirt => +1 
        // Child => +3
        // Money => +0
        // Profession => +0
        // Study => +0
        // Pet => +2
        // Malus => -1 à tous les autres joueur
        // Special => +1
    
    val myCards: PlayedHand = board.getOrElse(userId, Vector.empty)

    val baseScore = 
        myCards.foldLeft(0) {
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
            .filter((otherId, _) => otherId != userId)  
            .map { case (_, cards) => cards.count(_ == Card.Malus) } //compte le nombre de carte malus
            .sum

    baseScore - malusAgainstMe

def isTurnOf(userId: UserId, somethingToCheck: Any): Boolean =
    ??? // Pour toi Coco, faudra que tu changes State je pense :D

def gameIsOver(state: State): Boolean =
    ??? // (☞ﾟヮﾟ)☞ Pour toi Coco, moi je connais pas les règles 