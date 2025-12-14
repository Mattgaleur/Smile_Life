package apps.ul2025app68

extension (playedHand: PlayedHand)
    /** Checks whether the playedHand provides the given bonus.
    *
    * A bonus is considered present if:
    *  - it is granted by a [[Card.Profession]] currently in the hand, or
    *  - it is granted by a [[Card.Special]] card in the hand.
    *
    * @param bonus the bonus to look for
    * @return true if the bonus is present in the playedHand, false otherwise
    */
    def hasBonus(bonus: Bonus): Boolean = 
        require(playedHand != null, "playedHand must not be null")
        playedHand.exists:
            case p: Card.Profession => p.bonus.isDefined && p.bonus.get.exists(_==bonus)
            case Card.Special(thatBonus, _) => thatBonus == bonus 
            case _ => false 

    
    /** Computes the state of the hand after paying the given amount of money.
    *
    * The payment is made by selecting a subset of unused [[Card.Money]] cards
    * such that:
    *  - the total paid amount is greater than or equal to `amountToPay`,
    *  - the overpayment is minimal,
    *  - and, in case of a tie, the number of money cards used is minimal.
    *
    * The selected money cards are marked as `used` in the resulting hand.
    * All other cards remain unchanged.
    *
    * If the amount is zero or negative, the hand is returned unchanged.
    * If there is not enough unused money in the hand to pay the amount,
    * `None` is returned.
    *
    * @param amountToPay the amount of money to pay
    * @return
    *   - `Some(updatedHand)` if the payment can be made
    *   - `None` if the hand cannot pay the required amount
    */
    def handAfterPaying(amountToPay: Int): Option[PlayedHand] = 
        require(playedHand != null, "playedHand must not be null")
        if amountToPay <= 0 then
            Some(playedHand)
        else
            val availableMoney: List[(Int,Int)] = // (index in hand, money)
                playedHand.zipWithIndex.collect{
                    case (m @ Card.Money(amount, used), idx) if !used => // filter cards that are Money and not used
                        assert(amount > 0, s"Money amount should be > 0, got $amount")
                        (idx, amount)
                }.toList
            
            if availableMoney.isEmpty then None //if no money placed, no way to pay
            else
                val totalAvailable = availableMoney.map(_._2).sum // (idx,amount) so we take _.2 and sum to have all available money
                if totalAvailable < amountToPay then // if not enough money to pay, no way to pay
                    None
                else // if enough money to pay
                    assert(totalAvailable >= amountToPay)
                    var bestCombo: Option[(Int,List[(Int,Int)])] = None // (totalAvailable,List[(idx,amount)]), the bestCombo is None at first
                    for 
                        k <- 1 to availableMoney.length
                        combo <- availableMoney.combinations(k) //all combinations of the money we have possible
                    do
                        val sum = combo.map(_._2).sum
                        if sum >= amountToPay then
                            val overpay = sum - amountToPay
                            assert(overpay >= 0)
                            bestCombo match
                                case None => //initialized at None, updated in first iteration
                                    bestCombo = Some((sum,combo.toList))
                                case Some((bestSum,bestList)) => // compare with previous best way to pay to determine which is better
                                    val bestOverpay = bestSum - amountToPay
                                    if overpay < bestOverpay || (overpay == bestOverpay && combo.size < bestList.size) then 
                                        // if pay less payed or if same payed but higher cards (keep smallest for more possibilities)
                                        bestCombo = Some((sum,combo.toList))
                    assert(bestCombo.nonEmpty, "bestCombo should exist if totalAvailable >= amountToPay")
                    bestCombo match
                        case None => None
                        case Some((_,chosen)) => 
                            val indicesToUse: Set[Int]= chosen.map(_._1).toSet // Set of indexes of Money cards of the best way to pay
                            assert(indicesToUse.size == chosen.size)
                            assert(indicesToUse.forall(i => i >= 0 && i < playedHand.length))
                            val newHand: PlayedHand = 
                                playedHand.zipWithIndex.map {
                                    case (m @ Card.Money(amount,used), idx) if indicesToUse.contains(idx) => 
                                        assert(!used, s"Selected Money at idx=$idx was already used")
                                        m.copy(used = true) // if Money is used to pay here, copy but used = True now
                                    case (card, _) => card  // cards that are not money stay the same
                                }
                            assert(newHand.length == playedHand.length, "hand size must not change")
                            Some(newHand)

