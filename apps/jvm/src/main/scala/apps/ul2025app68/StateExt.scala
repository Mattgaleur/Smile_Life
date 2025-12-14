package apps.ul2025app68

import scala.collection.immutable.Queue
import cs214.webapp.UserId

extension (state: State)
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
     * @return
     *   A new state where:
     *     - `playerQueue` has been rotated to the next valid player
     *     - skipped players had one skip-malus removed from their board
     *     - all other fields remain the same except the updated board/piles.
     */
    def toNextPlayer: State =
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

                if b.hasSkipMalus(current) then
                    val (newBoard,newCardPiles) = b.inflictSkipMalus(piles,current)
                    b = newBoard
                    piles = newCardPiles
                    remaining -= 1
                else
                    done = true

            State(hands, b, piles, queue, log)

    /** Checks whether it is currently `userId`'s turn to play.
     *
     * The rule is simple: the player who is at the head of `playerQueue`
     * is the only one allowed to perform an action.
     *
     * @param userId
     *   The player who is trying to act.
     * @return
     *   True iff `userId` is the head of the queue.
     */
    def isTurnOf(userId: UserId): Boolean =
        state.playerQueue.head == userId

