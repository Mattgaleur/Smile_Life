package apps.ul2025app68

import cs214.webapp.UserId

extension (cardPiles: CardPiles)
    /** Discards a card by moving it to the trash pile.
    *
    * @param card the card to discard
    * @return a new [[CardPiles]] with the card added on top of the trash pile
    */
    def discard(card: Card): CardPiles =
        cardPiles.copy(
            trashPile = card :: cardPiles.trashPile
        )

    /** Picks the top card from either the default pile or the trash pile.
        *
        * @param fromDefaultPile if true, pick from the default pile;
        *                        otherwise pick from the trash pile
        * @return
        *   - `Some((card, updatedPiles))` if the chosen pile is non-empty
        *   - `None` if the chosen pile is empty
        */
    def pickCard(fromDefaultPile: Boolean): Option[(Card, CardPiles)] =
        val CardPiles(defaultPile, trashPile) = cardPiles
        if fromDefaultPile && defaultPile.nonEmpty then
            Some(
                defaultPile.head,
                cardPiles.copy(defaultPile = defaultPile.tail)
            )
        else if !fromDefaultPile && trashPile.nonEmpty then
            Some(
                trashPile.head,
                cardPiles.copy(trashPile = trashPile.tail)
            )
        else
            None

    /** Deals cards to the given clients from the default pile.
        *
        * Each client receives [[DEFAULT_CARD_IN_HAND]] cards, in the order
        * provided by the default pile. Cards are assigned sequentially
        * following the order of `clients`.
        *
        * The remaining cards stay in the default pile, and the trash pile
        * is reset to empty.
        *
        * @param clients the users to whom cards are dealt
        * @return a tuple containing:
        *   - a map from [[UserId]] to the dealt [[Hand]]
        *   - the updated [[CardPiles]]
        */
    def giveCardsTo(clients: Seq[UserId]): (Map[UserId, Hand], CardPiles) =
        val CardPiles(defaultPile, trashPile) = cardPiles
        val hands: Map[UserId, Hand] =
        clients
            .zip(defaultPile.grouped(DEFAULT_CARD_IN_HAND))
            .map { case (id, cards) => id -> cards.toVector }
            .toMap

        val piles: CardPiles =
        CardPiles(
            defaultPile.drop(clients.length * DEFAULT_CARD_IN_HAND),
            List.empty
        )

        (hands, piles)

