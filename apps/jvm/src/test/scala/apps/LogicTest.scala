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

    // ## END OF GAME TESTS

    // I don't really know how the games ends so I let it to you