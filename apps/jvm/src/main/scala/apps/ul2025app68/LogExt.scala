package apps.ul2025app68

import cs214.webapp.UserId

extension (log: Log)
    /** Appends a human-readable log entry describing a user event.
    *
    * The method formats a textual message based on the given [[Event]]
    * and prepends it to the current log.
    *
    * A visual separation marker is inserted before the message when
    * the event ends the current player's turn. This helps group log
    * entries by turn for readability.
    *
    * @param userId the user who triggered the event
    * @param event the event to log
    * @return a new [[Log]] with the formatted entry prepended
    */
    def write(userId: UserId)(event: Event): Log =
        val message = event match
            case Event.Discard(card) => 
                f"$userId discarded a ${card.productPrefix} card"

            case Event.PickCard(isDefaultPile) =>
                if isDefaultPile then
                    f"$userId drew a card from the Pile"
                else 
                    f"$userId drew a card from the Trash Pile"

            case Event.QuitJob =>
                f"$userId decided to quit his job"

            case Event.EndGame =>
                f"$userId is tired of playing, he ended the game"

            case Event.PlayCard(card, selectedUser) => card match
                case Card.MalusCard(malus) => 
                    f"$userId used $malus on $selectedUser" 
                case Card.Travel(price) =>
                    f"$userId planned a Travel for $$$price"
                case Card.House(price) =>
                    f"$userId bought a House for $$$price"
                case Card.Special(bonus, name) =>
                    f"$userId played the $name card"
                case Card.Money(amount, used) =>
                    f"$userId earned $$$amount worth of Money"
                case Card.Profession(studyRequired, salary, bonus, name) =>
                    f"$userId became a $name"
                case Card.Flirt =>
                    f"$userId decided to Flirt"
                case Card.Marriage =>
                    f"$userId married a random person, congrats!"
                case Card.Child =>
                    f"$userId had a child with his partner"
                case Card.Study =>
                    f"$userId spent one year to Study"
                case Card.Pet =>
                    f"$userId bought a Pet"
        event match
            case Event.Discard(_) | Event.PlayCard(_, _) | Event.QuitJob =>
                Log.separation :: message :: log
            case Event.PickCard(_) | Event.EndGame =>
                message :: log