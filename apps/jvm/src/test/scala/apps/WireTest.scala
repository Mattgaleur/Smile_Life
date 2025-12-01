package apps.ul2025app68

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

import scala.util.Random
import cs214.webapp.server.StateMachine
import apps.ul2025app68.PhaseView.GameView
import apps.ul2025app68.PhaseView.VictoryView
import apps.ul2025app68.Card.*

class WireTests extends WebappSuite[Event, State, View]:

    val sm = Logic()

    test("Event Wire: For cards that has no attributes"):
        val seed = 2
        val rand = new Random(seed)
        val nbOfTest = 10

        val randomCards =
            List.fill(5)(Card.fromOrdinal(rand.nextInt(5)))
        
        // println("Discard and PlayCard Tests")
        for card <- randomCards do
            Event.Discard(card).testEventWire
            Event.PlayCard(card).testEventWire
        
        val listOfRandomCardsList = List.fill(nbOfTest)(
            List.fill(5)(Card.fromOrdinal(rand.nextInt(5)))
        )

        // println("PickCard Tests")
        Event.PickCard(true).testEventWire
        Event.PickCard(false).testEventWire

        // for cards <- listOfRandomCardsList do // Maybe Useful for later implementation of PickCards
        //     Event.PickCard(DefaultPile(cards)).testEventWire
        //     Event.PickCard(DiscardPile(cards)).testEventWire

    test("Card Wire: For Money and Profession"):
        val seed = 2
        val rand = new Random(seed)
        val randomInts: List[List[Int]] =
            List.fill(2)(List.fill(5)(rand.nextInt(Int.MaxValue)))
        val randomMoney = for amount <- randomInts(0) yield Money(amount) 
        val randomProfession = for (studyR, salary) <- randomInts(0).zip(randomInts(1))
            yield Profession(studyR, salary) 

        
        for 
            (money, job) <- randomMoney.zip(randomProfession) 
        do
            val moneyResult = CardWire.decode(CardWire.encode(money))
            val jobResult = CardWire.decode(CardWire.encode(job))
            assert(moneyResult.isSuccess)
            assert(moneyResult.get == money)
            assert(jobResult.isSuccess)
            assert(jobResult.get == job)

    test("View Wire: For cards that has no attributes"):
        val seed = 2
        val rand = Random(seed)
        val nbOfSample = 10

        val listOfRandomCardsVector = List.fill(nbOfSample)(
            Vector.fill(rand.nextInt(15))(Card.fromOrdinal(rand.nextInt(5)))
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
            val winners: List[UserId] = 
                for 
                    i <- (1 to nbOfUser).toList
                yield
                    rand.nextString(rand.nextInt(10))
            
            View(GameView(
                randomBoard,
                listOfRandomCardsVector(rand.nextInt(10))
            )).testViewWire

            View(VictoryView(winners))
