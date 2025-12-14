package apps.ul2025app68

import cs214.webapp.UserId
import cs214.webapp.IllegalMoveException

extension (board: Board)
    /** Places a card onto a player's played hand (their board).
     *
     * The card is appended at the end of the player's vector of played cards.
     *
     * @param card
     *   The card to place.
     * @param userId
     *   The player receiving the card (could be self or another player, depending on rules).
     * @return
     *   A new board where `card` has been appended to `userId`'s played hand.
     */
    def placeCard(card: Card, userId: UserId): Board =
        board.updated(
            userId, card +: board.get(userId).get 
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
     * @param all
     *   If true, remove all matching cards; otherwise remove only one.
     * @throws IllegalMoveException
     *   If no matching card exists in the target player's played hand.
     * @return
     *   A new board where the matching card(s) have been removed from `userId`'s played hand.
     */
    def removeCard(userId: UserId, all: Boolean = false)(identifier: Card => Boolean): Board =
        val playedHand = board.get(userId).get 
        if !playedHand.exists(identifier) then
            throw IllegalMoveException("An error occured, you can't remove the card you want")
        else if all then
            board.updated(
                userId, playedHand.filterNot(identifier)
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
     * @return
     *   A Map associating each player with their number of points.
     */
    def countSmilesMap: Map[UserId, Int] =
        board.keySet.map(userId =>
            (userId, board(userId).map(card => card.smileValue).sum)
        ).toMap
        
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
    def hasSkipMalus(userId : UserId): Boolean = 
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
    def inflictSkipMalus(cardPiles: CardPiles, userId: UserId): (Board, CardPiles) =
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
