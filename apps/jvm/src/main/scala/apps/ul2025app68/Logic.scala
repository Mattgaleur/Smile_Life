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
        event match
            case Event.Discard(card) =>
                val cardInHand: Vector[Card] = hands.get(userId).get  
                if cardInHand.length == MIN_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalMoveException("You haven't picked a card yet")
                else if cardInHand.length < MIN_NUMBER_OF_CARD_IN_HAND then
                    throw IllegalStateException(
                        f"Impossible situation happened: you have less cards than ${MIN_NUMBER_OF_CARD_IN_HAND} cards"
                    )
                else 
                    val newHands: Map[UserId, Hand] = hands.updated(
                        userId, cardInHand.drop(cardInHand.indexOf(card))
                    )
                    Seq(
                        Render(state.copy(
                            hands = newHands,
                            cardPiles = cardPiles.discard(card)
                        ))
                    )
            case Event.PlayCard(card) =>
                ???
            case Event.PickCard(isDefaultPile) =>
                if isDefaultPile then
                    ???
                else 
                    ???
        

    override def project(state: State)(userId: UserId): View = 
        val State(hands, board, cardPiles) = state
        hands.get(userId) match
            case Some(hand) => 
                View(
                    board = board,
                    hand = hand    
                )
            case None =>
                throw IllegalArgumentException(f"The given userId ${userId} is unkown")
        

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
    val defaultPile: Pile = ??? // Pour toi Coco ;)
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
    ??? // Pour toi Coco ;)

    // mon idée pour la version 1 qui sera très simplifier:
        // Flirt => +1 
        // Child => +3
        // Money => +0
        // Profession => +0
        // Study => +0
        // Pet => +2
        // Malus => -1 à tous les autres joueur
        // Special => +1