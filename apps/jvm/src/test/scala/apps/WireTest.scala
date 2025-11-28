package apps.ul2025app68

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

import scala.util.Random
import cs214.webapp.server.StateMachine

class WireTests extends WebappSuite[Event, State, View]:

    val sm = Logic()

    test("Event Wire: Encode then decode outputs the same as the input"):
        val seed = 2
        val rand = new Random(seed)
        val nbOfTest = 10

        val randomCards =
            List.fill(5)(Card.fromOrdinal(rand.nextInt(8)))
        
        // println("Discard and PlayCard Tests")
        for card <- randomCards do
            Event.Discard(card).testEventWire
            Event.PlayCard(card).testEventWire
        
        val listOfRandomCardsList = List.fill(nbOfTest)(
            List.fill(5)(Card.fromOrdinal(rand.nextInt(8)))
        )

        // println("PickCard Tests")
        Event.PickCard(true).testEventWire
        Event.PickCard(false).testEventWire

        // for cards <- listOfRandomCardsList do // Maybe Useful for later implementation of PickCards
        //     Event.PickCard(DefaultPile(cards)).testEventWire
        //     Event.PickCard(DiscardPile(cards)).testEventWire
            

    test("View Wire: Encode then decode outputs the same as the input"):
        val seed = 2
        val rand = Random(seed)
        val nbOfSample = 10

        val listOfRandomCardsVector = List.fill(nbOfSample)(
            Vector.fill(rand.nextInt(15))(Card.fromOrdinal(rand.nextInt(8)))
        )

        for 
            nbOfUser <- (1 to 4)
        do
            val randomBoard: Board = (
                for 
                    i <- (1 to nbOfUser)
                yield 
                    (rand.nextString(rand.nextInt(10)),listOfRandomCardsVector(rand.nextInt(nbOfSample)))
            ).toMap
            View(
                randomBoard,
                listOfRandomCardsVector(rand.nextInt(10))
            ).testViewWire
