package apps.ul2025app68

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

import scala.util.Random
import cs214.webapp.server.StateMachine
import cs214.webapp.Action.Render
import scala.util.Try
import Card.*
import munit.FunSuite
import scala.collection.mutable.Queue
import PhaseView.*

import Event.*

class LogicTests extends WebappSuite[Event, State, View]:
    val sm = Logic()
    val seed = 2
    given rand: Random = Random(seed)

    extension (state: State)
        def getUserToPlay: UserId =
            state.playerQueue.head

        def play(event: Event): State =
            sm.transition(state)(state.getUserToPlay, event).getState

        def getCardToPlay(using rand: Random): Card =
            state.hands.get(getUserToPlay).get.apply(rand.nextInt(MAX_NUMBER_OF_CARD_IN_HAND))

        def getCardToPlay(index: Int): Card =
            state.hands.get(getUserToPlay).get.apply(index)

    extension (board: Board)
        def contains(card: Card)(userId: UserId): Boolean =
            board.get(userId).get.exists(card == _)

    extension (seq: Try[Seq[Action[State]]])
        def getState: State =
            seq.get.collect {case render: Render[State] => render.st} .lastOption.get


    // ## INITIAL STATE TESTS
    lazy val initState = sm.init(USER_IDS)

    test("Initial State: all player have the right number of card"):
        initState.hands.foreach: (userId, hand) =>
            assert(hand.length == MIN_NUMBER_OF_CARD_IN_HAND)
    
    test("Initial State: the cards that the player have are removed from the DefaultPile"):
        val initPile = setPiles(rand)
        val (hands, newPile) = initPile.giveCardsTo(USER_IDS)

        val givenCards = hands.values.flatten.toList

        assert(givenCards ++ newPile.defaultPile == initPile.defaultPile)


    test("Initial State: the DefaultPile is not empty and the DiscardPile is"):
        // Picking Card from the draw pile should give a card
        assert(!initState.cardPiles.defaultPile.isEmpty)
        // Picking Card from the trash pile should give nothing
        assert(initState.cardPiles.trashPile.isEmpty)

    
    test("Initial State: the Board is Empty"):
        initState.board.foreach: (userId, playedHand) =>
            assert(playedHand.isEmpty)

    // ## EVENT TESTS
    lazy val readyToPlayState: State = initState.play(PickCard(true))
    

    test("Event: PickCard remove card from the relevant Pile"):
        val initCardPiles = CardPiles(
            List(Flirt, Study, Study),
            List(Pet, Child, Study)
        )
        val tookFromDrawPile = CardPiles(
            List(Study, Study),
            List(Pet, Child, Study)
        )
        val tookFromTrashPile = CardPiles(
            List(Flirt, Study, Study),
            List(Child, Study)
        )
        val state = initState.copy(cardPiles = initCardPiles)

        assert(state.play(PickCard(true)).cardPiles == state.copy(cardPiles = tookFromDrawPile).cardPiles)
        assert(state.play(PickCard(false)).cardPiles == state.copy(cardPiles = tookFromTrashPile).cardPiles)

    test("Event: PlayCard updates the Board with the played card"):
        val nbOfTest = 10
        for index <- (0 until MAX_NUMBER_OF_CARD_IN_HAND) do
            val cardToPlay = readyToPlayState.getCardToPlay(index)
            val newState = readyToPlayState.play(PlayCard(cardToPlay))
            assert(newState.board.contains(cardToPlay)(readyToPlayState.getUserToPlay))

    test("Event: Discard add the given card to the trash pile"):
        val cardToPlay = readyToPlayState.getCardToPlay
        val oldTrashPile = readyToPlayState.cardPiles.trashPile
        val newTrashPile = readyToPlayState.play(Discard(cardToPlay)).cardPiles.trashPile
        
        assert(cardToPlay :: oldTrashPile == newTrashPile)

    test("Event: The player shouldn't be able to PickCard from an empty Pile"):
        val state = initState.copy(
            cardPiles = CardPiles(List.empty, List.empty)
        )
        assertFailure[IllegalMoveException]:
            sm.transition(state)(state.getUserToPlay, PickCard(true))

        assertFailure[IllegalMoveException]:
            sm.transition(state)(state.getUserToPlay, PickCard(false))

    test("Event: The first player in the queue is the only one who can play"):
        val userToPlay = initState.playerQueue.head
        val otherUsers = initState.playerQueue.tail

        // shouldn't throw
        assert(sm.transition(initState)(userToPlay, PickCard(true)).isSuccess)

        for userId <- otherUsers do
            assertFailure[IllegalMoveException]:
                sm.transition(initState)(userId, PickCard(true))


    // ## CARD RULES TESTS: Money & Profession ################################

    test("Money.canBePlaced: true when there is a profession with sufficient salary") {
        val playedHand: PlayedHand =
            Vector(
                Study,                      // irrelevant for Money.canBePlaced
                Profession(studyRequired = 1, salary = 4)
            )

        val card = Money(3) // amount <= salary (3 <= 4)

        assert(card.canBePlaced(playedHand),
            "Money(3) should be placeable when a Profession with salary 4 is present")
    }

    test("Money.canBePlaced: false when no profession is present") {
        val playedHand: PlayedHand =
            Vector(
                Study,
                Study,
                Pet
            )

        val card = Money(2)

        assert(!card.canBePlaced(playedHand),
            "Money should not be placeable when no Profession is present")
    }

    test("Money.canBePlaced: false when profession salary is too low") {
        val playedHand: PlayedHand =
            Vector(
                Study,
                Profession(studyRequired = 1, salary = 2)
            )

        val card = Money(3) // amount > salary (3 > 2)

        assert(!card.canBePlaced(playedHand),
            "Money(3) should NOT be placeable when the only Profession has salary 2")
    }

    test("Profession.canBePlaced: true when enough Study and no existing Profession") {
        val playedHand: PlayedHand =
            Vector(
                Study,
                Study,
                Study
            )

        val card = Profession(studyRequired = 2, salary = 4)

        assert(card.canBePlaced(playedHand),
            "Profession(2,5) should be placeable with 3 Study cards and no existing Profession")
    }

    test("Profession.canBePlaced: false when not enough Study") {
        val playedHand: PlayedHand =
            Vector(
                Study // only 1 Study
            )

        val card = Profession(studyRequired = 2, salary = 5)

        assert(!card.canBePlaced(playedHand),
            "Profession(2,5) should NOT be placeable with only 1 Study card")
    }

    test("Profession.canBePlaced: false when there is already a Profession") {
        val playedHand: PlayedHand =
            Vector(
                Study,
                Study,
                Profession(studyRequired = 1, salary = 3)
            )

        val card = Profession(studyRequired = 2, salary = 6)

        assert(!card.canBePlaced(playedHand),
            "A second Profession should NOT be placeable when one Profession is already in the played hand")
    }


    // ## TURNS AND QUEUE TESTS 

    test("toNextPlayer rotates the queue (1,2,3) -> (2,3,1)") {
        val q = Queue.from(USER_IDS)
        val rotated = toNextPlayer(q)

        assertEquals(rotated.toList, USER_IDS.tail :+ USER_IDS.head)
        // original queue should not be modified
        assertEquals(q.toList, USER_IDS.toList)
    }

    test("toNextPlayer on empty queue returns empty queue") {
        val emptyQ = Queue.empty[UserId]
        val rotated = toNextPlayer(emptyQ)

        assert(rotated.isEmpty)
    }

    test("isTurnOf is true only for the head of the queue") {
        val q = Queue.from(USER_IDS)
        val head = q.head
        val others = q.tail

        assert(isTurnOf(head, q))

        for user <- others do
            assert(!isTurnOf(user, q))
    }

    
    // ## COUNTSMILES TESTS

    test("countSmiles: basic scoring without malus") {
        val user = USER_IDS.head

        val board: Board = Map(
            user -> Vector(
                Flirt,   // +1
                Child,   // +3
                Pet,     // +2
                Special  // +1
            )
        )

        val scores = countSmiles(board)

        assertEquals(scores(user), 1 + 3 + 2 + 1)
    }

    test("countSmiles: malus applied from other players") {
        val u1 = USER_IDS.head
        val u2 = USER_IDS.tail.head

        val board: Board = Map(
            u1 -> Vector(Flirt, Pet),          // base: 1 + 2 = 3
            u2 -> Vector(Malus, Malus, Flirt)  // 2 malus against u1
        )

        val scoreU1 = countSmiles(board, u1)
        val scoreU2 = countSmiles(board, u2)

        // u1: 3 base - 2 malus = 1
        assertEquals(scoreU1, 1)

        // u2: only Flirt counts, Malus gives 0 points to owner
        assertEquals(scoreU2, 1)
    }


    // SETPILES TESTS 

    test("setPiles: defaultPile has requested size") {
        val size = 50
        val piles = setPiles(rand, size)

        assertEquals(piles.defaultPile.size, size)
    }

    test("setPiles: generated Money and Profession cards have parameters in expected ranges") {
        val size = 200
        val piles = setPiles(rand, size)

        piles.defaultPile.foreach {
            case Money(amount) =>
                assert(amount >= 1 && amount <= 4, s"Money amount out of range: $amount")

            case Profession(studyRequired, salary) =>
                // according to setPiles: between(1,7) and between(1,5)
                assert(studyRequired >= 1 && studyRequired <= 6,
                    s"Profession.studyRequired out of range: $studyRequired")
                assert(salary >= 1 && salary <= 4,
                    s"Profession.salary out of range: $salary")

            case _ => // other cards: nothing to check
        }
    }

    

    // ## END OF GAME TESTS
    // I don't really know how the games ends so I let it to you

    test("gameIsOver is false when draw pile is not empty") {
        val piles = CardPiles(
            defaultPile = List(Flirt, Study),
            trashPile = Nil
        )

        assert(!gameIsOver(piles))
    }

    test("gameIsOver is true when draw pile is empty") {
        val piles = CardPiles(
            defaultPile = Nil,
            trashPile = List(Flirt, Study)
        )

        assert(gameIsOver(piles))
    }

    test("project returns VictoryView when game is over") {
        // simple board with one clear winner
        val winner = USER_IDS.head
        val loser  = USER_IDS.tail.head

        val board: Board = Map(
            winner -> Vector(Flirt, Child), // 1 + 3 = 4
            loser  -> Vector(Flirt)        // 1
        )

        val state = State(
            hands = Map.empty,
            board = board,
            cardPiles = CardPiles(Nil, Nil), // draw pile empty -> game over
            playerQueue = Queue.from(USER_IDS)
        )

        val view = sm.project(state)(winner)

        view.phaseView match
            case VictoryView(winners) =>
                assertEquals(winners, List(winner))
            case other =>
                fail(s"Expected VictoryView, got $other")
    }

    test("transition throws when trying to play after game is over") {
        val state = State(
            hands = Map(USER_IDS.head -> Vector(Flirt)),
            board = Map(USER_IDS.head -> Vector.empty),
            cardPiles = CardPiles(Nil, Nil), // no cards -> game over
            playerQueue = Queue.from(USER_IDS)
        )

        assertFailure[IllegalMoveException]:
            sm.transition(state)(state.getUserToPlay, PlayCard(Flirt))
    }