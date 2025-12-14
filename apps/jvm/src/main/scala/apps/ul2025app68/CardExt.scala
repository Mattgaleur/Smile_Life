package apps.ul2025app68

import apps.ul2025app68.Card.*

val MAX_NUMBER_OF_STUDY = 6
val MAX_NUMBER_OF_FLIRT = 5
extension (card: Card)

    /** Determines whether this card can be legally placed in the given played hand.
    *
    * The rules depend on the concrete type of the card:
    *
    *  - [[Money]]:
    *    Can be placed only if the hand already contains a [[Profession]]
    *    whose salary is greater than or equal to the money amount.
    *
    *  - [[Profession]]:
    *    Can be placed only if:
    *      - the hand does not already contain another profession, and
    *      - the number of [[Study]] cards (possibly doubled by
    *        [[Bonus.DoubleStudy]]) meets the required study level.
    *
    *  - [[Study]]:
    *    Can be placed only if:
    *      - the number of study cards is below the limit (6), unless
    *        [[Bonus.UnlimitedStudy]] is present, and
    *      - the player is not currently working, unless
    *        [[Bonus.StudyWhileWorking]] is present.
    *
    *  - [[Flirt]]:
    *    Can be placed only if:
    *      - the number of flirt cards is below the limit (5), unless
    *        [[Bonus.UnlimitedFlirt]] is present, and
    *      - the player is not married, unless
    *        [[Bonus.FlirtWhileMarried]] is present.
    *
    *  - [[Marriage]]:
    *    Can be placed only if:
    *      - at least one [[Flirt]] is present, and
    *      - no marriage is already present,
    *      - unless [[Bonus.DoubleMarriage]] allows a second marriage.
    *
    *  - [[Child]]:
    *    Can be placed only if the hand already contains a [[Marriage]].
    *
    *  - [[MalusCard]]:
    *    Can be placed only if the corresponding malus is not protected
    *    by a matching [[Bonus.MalusProtection]] and its application
    *    conditions are satisfied (e.g. profession required for
    *    dismissal or burn-out, marriage required for divorce, etc.).
    *
    *  - [[Pet]] and [[Special]]:
    *    Can always be placed.
    *
    *  - [[House]] and [[Travel]]:
    *    Can be placed only if:
    *      - the corresponding free bonus is present, or
    *      - the hand contains enough money to pay the price.
    *
    * @param playedHand the current hand into which the card would be placed
    * @return true if the card can be placed according to the game rules,
    *         false otherwise
    */
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
            def withinLimit = (playedHand.count(_ == Study) < MAX_NUMBER_OF_STUDY) || playedHand.hasBonus(Bonus.UnlimitedStudy)
            def isNotWorking = !playedHand.exists(_.isInstanceOf[Profession]) || playedHand.hasBonus(Bonus.StudyWhileWorking)
            withinLimit && isNotWorking

        case Flirt => 
            val flirts = playedHand.count(_ == Flirt)
            def withinLimit = (flirts < MAX_NUMBER_OF_FLIRT) || playedHand.hasBonus(Bonus.UnlimitedFlirt)
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
