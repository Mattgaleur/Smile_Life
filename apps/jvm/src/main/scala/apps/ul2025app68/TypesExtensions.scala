package apps.ul2025app68

import apps.ul2025app68.Card.*
import cs214.webapp.UserId
import scala.collection.immutable.Queue

// ######### Card #########
extension (card: Card)
    def canBePlaced(playedHand: PlayedHand): Boolean = card match
        case Money(amount,_) =>
            playedHand.collectFirst {case p: Profession => p} match
                case Some(profession) => profession.salary >= amount 
                case None => false
            
        case Profession(studyRequired, salary,_,_) =>
            val studyMultiplier = if playedHand.hasBonus(Bonus.DoubleStudy) then 2 else 1
            val studies = playedHand.count(_ == Study) * studyMultiplier
            val enoughStudy =  studies >= studyRequired
            val isJobLess = !playedHand.exists(_.isInstanceOf[Profession])
            isJobLess && enoughStudy

        case Study =>
            def withinLimit = (playedHand.count(_ == Study) < 6) || playedHand.hasBonus(Bonus.UnlimitedStudy)
            def isNotWorking = !playedHand.exists(_.isInstanceOf[Profession]) || playedHand.hasBonus(Bonus.StudyWhileWorking)
            withinLimit && isNotWorking

        case Flirt => 
            val flirts = playedHand.count(_ == Flirt)
            def withinLimit = (flirts < 5) || playedHand.hasBonus(Bonus.UnlimitedFlirt)
            def isNotMarried = !playedHand.exists(_ == Marriage) || playedHand.hasBonus(Bonus.FlirtWhileMarried)
            withinLimit && isNotMarried
            
        case Marriage => 
            val nbOfFlirt = playedHand.count(_ == Flirt)
            val nbOfMarriage = playedHand.count(_ == Marriage)
            
            (playedHand.hasBonus(Bonus.DoubleMarriage) && (nbOfFlirt >= 1) && (playedHand.count(_ == Marriage) < 2)) || 
            (nbOfFlirt != 0 && nbOfMarriage == 0)
                

        case Child => 
            playedHand.exists(_ == Marriage)

        case MalusCard(malus) => 
            def hasNoProtection = !playedHand.hasBonus(Bonus.MalusProtection(malus))
            def canApply = malus match
                case Malus.Disease => true
                case Malus.Accident => true
                case Malus.BurnOut => playedHand.exists(_.isInstanceOf[Profession])
                case Malus.Tax => playedHand.exists:
                    case m: Money => !m.used
                    case _ => false
                case Malus.Divorce => playedHand.exists(_ == Marriage)
                case Malus.Dismissal => playedHand.exists(_.isInstanceOf[Profession])
                case Malus.TerroristAttack => playedHand.exists(_ == Child)
                case Malus.RepeatYear => playedHand.exists(_ == Study) && !playedHand.exists(_.isInstanceOf[Profession])
            
            hasNoProtection && canApply

        case Pet => true
            
        case Special(bonus, _) => true

        case House(price) => 
            def hasBonus = playedHand.hasBonus(Bonus.FreeHouse)
            def hasMoney = playedHand.handAfterPaying(price).isDefined
            hasBonus || hasMoney

        case Travel(price) => 
            def hasBonus = playedHand.hasBonus(Bonus.FreeTravel)
            def hasMoney = playedHand.handAfterPaying(price).isDefined
            hasBonus || hasMoney 



// ######### PlayedHand #########
extension (playedHand: PlayedHand)
    def hasBonus(bonus: Bonus): Boolean = playedHand.exists:
        case p: Card.Profession => p.bonus.isDefined && p.bonus.get.exists(_==bonus)
        case Card.Special(thatBonus, _) => thatBonus == bonus 
        case _ => false 

    def handAfterPaying(amountToPay: Int): Option[PlayedHand] = 
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



// ######### CardPiles #########
extension (cardPiles: CardPiles)
    def discard(card: Card): CardPiles =
        cardPiles.copy(
            trashPile = card :: cardPiles.trashPile
        )

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
    
    def giveCardsTo(clients: Seq[UserId])(using nbOfCards: Int): (Map[UserId, Hand], CardPiles) =
        val CardPiles(defaultPile, trashPile) = cardPiles
        val hands: Map[UserId, Hand] =
            clients
                .zip(defaultPile.grouped(nbOfCards))   // gives (userId, cards)
                .map { case (id, cards) => id -> cards.toVector }
                .toMap

        val piles: CardPiles = CardPiles(defaultPile.drop(clients.length * nbOfCards), List.empty)
        (hands, piles)

// ######### State #########
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
     * @param playerQueue
     *   The queue representing turn order. The head is the current player.
     * @return
     *   True iff `userId` is the head of the queue.
     */
    def isTurnOf(userId: UserId): Boolean =
        state.playerQueue.head == userId


// ######### Log #########
extension (log: Log)
    def write(userId: UserId)(event: Event): Log =
        event match
            case Event.Discard(card) => 
                f"$userId discarded a ${card.productPrefix} card" :: log

            case Event.PickCard(isDefaultPile) =>
                if isDefaultPile then
                    f"$userId drew a card from the Pile" :: log
                else 
                    f"$userId drew a card from the Trash Pile" :: log

            case Event.QuitJob =>
                f"$userId decided to quit his job" :: log

            case Event.EndGame =>
                f"$userId is tired of playing, he ended the game" :: log

            case Event.PlayCard(card, selectedUser) => card match
                case Card.MalusCard(malus) => 
                    f"$userId used $malus on $selectedUser" :: log 
                case Card.Travel(price) =>
                    f"$userId planned a Travel for $price" :: log
                case Card.House(price) =>
                    f"$userId bought a House for $price" :: log
                case Card.Special(bonus, name) =>
                    f"$userId played the $name" :: log
                case Card.Money(amount, used) =>
                    f"$userId earned $amount Money" :: log
                case Card.Profession(studyRequired, salary, bonus, name) =>
                    f"$userId became a $name" :: log
                case Card.Flirt =>
                    f"$userId decided to Flirt" :: log
                case Card.Marriage =>
                    f"$userId married a random person, congrats!" :: log
                case Card.Child =>
                    f"$userId had a child with his partner" :: log
                case Card.Study =>
                    f"$userId spent one year to Study" :: log
                case Card.Pet =>
                    f"$userId bought a Pet" :: log