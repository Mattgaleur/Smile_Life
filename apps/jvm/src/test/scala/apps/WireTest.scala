package apps.ul2025app68

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite
import org.scalacheck.{Prop, Test}
import cs214.webapp.server.StateMachine
import scala.util.Random

import apps.ul2025app68.PhaseView.GameView
import apps.ul2025app68.PhaseView.VictoryView
import apps.ul2025app68.Card.*
import scala.util.Try

class WireTests extends WebappSuite[Event, State, View]:

    val sm = Logic()

    def assertProp[A](prop: Prop): Unit =
        if (!Test.check(Test.Parameters.default, prop).passed) then
            val result = Test.check(Test.Parameters.default, prop)
            throw new AssertionError(
                "ScalaCheck property failed!\n" + 
                f"Message: ${result.toString()}"
            )

    test("Card Wire: Encoding and Decoding tests with ScalaCheck"):
        assertProp(
            Prop.forAll { (card: Card) =>
                Try {testWire(CardWire)(card)}.isSuccess
            }
        )

    test("Event Wire: Event.PlayCard encoding works"):
        assertProp(
            Prop.forAll { (card: Card, userId: UserId) =>
                Try {Event.PlayCard(card, userId).testEventWire}.isSuccess
            }
        )

    test("Event Wire: Event.Discard encoding works"):
        assertProp(Prop.forAll { (card: Card) =>
            Try {Event.Discard(card).testEventWire}.isSuccess
        })

    test("Event Wire: Event.PickCard encoding works"):
        Event.PickCard(true).testEventWire
        Event.PickCard(false).testEventWire
    
    test("Event Wire: Event.QuitJob encoding works"):
        Event.QuitJob.testEventWire

    test("Event Wire: Event.EndGame encoding works"):
        Event.EndGame.testEventWire

    test("View Wire: PhaseView.GameView enconding works"):
        assertProp(
            Prop.forAll { (
                board: Board,
                hand: Hand,
                lastDiscard: Option[Card],
                turnOf: UserId,
                defaultPileSize: Int,
                log: Log
            ) =>
                Try {View(PhaseView.GameView(
                    board: Board, hand: Hand, lastDiscard: Option[Card], turnOf: UserId, defaultPileSize: Int, log: Log
                ))}.isSuccess
            }
        )


    test("View Wire: PhaseView.Victory enconding works"):
        assertProp(
            Prop.forAll { (winners: Seq[UserId], board: Board, log: Log) =>
                Try {View(PhaseView.VictoryView(winners, board, log))}.isSuccess
            }
        )

    test("Legacy Event Wire: For cards that has no attributes"):
        val seed = 2
        val rand = new Random(seed)
        val nbOfTest = 10

        val randomCards =
            List.fill(5)(Card.fromOrdinal(rand.nextInt(5)))
        
        for card <- randomCards do
            Event.Discard(card).testEventWire
            Event.PlayCard(card, rand.nextString(10)).testEventWire
        
        val listOfRandomCardsList = List.fill(nbOfTest)(
            List.fill(5)(Card.fromOrdinal(rand.nextInt(5)))
        )

        Event.PickCard(true).testEventWire
        Event.PickCard(false).testEventWire


    test("Legacy Card Wire: For Money and Profession"):
        val seed = 2
        val rand = new Random(seed)
        val randomInts: List[List[Int]] =
            List.fill(2)(List.fill(5)(rand.nextInt(Int.MaxValue)))
        val randomMoney = for amount <- randomInts(0) yield Money(amount) 
        val randomProfession = for (studyR, salary) <- randomInts(0).zip(randomInts(1))
            yield Profession(studyR, salary, None, "") 

        
        for 
            (money, job) <- randomMoney.zip(randomProfession) 
        do
            val moneyResult = CardWire.decode(CardWire.encode(money))
            val jobResult = CardWire.decode(CardWire.encode(job))
            assert(moneyResult.isSuccess)
            assert(moneyResult.get == money)
            assert(jobResult.isSuccess)
            assert(jobResult.get == job)

    test("Legacy View Wire: For cards that has no attributes"):
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
                listOfRandomCardsVector(rand.nextInt(10)),
                None,
                rand.nextString(5),
                rand.nextInt(),
                log = List.empty
            )).testViewWire

            View(VictoryView(winners,Map.empty,List.empty))
