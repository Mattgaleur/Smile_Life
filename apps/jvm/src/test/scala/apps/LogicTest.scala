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
      seq.get.collect { case render: Render[State] => render.st }.lastOption.get

  // ## INITIAL STATE TESTS
  lazy val initState = sm.init(USER_IDS)

  test("Initial State: all player have the right number of card"):
    initState.hands.foreach: (userId, hand) =>
      assertEquals(hand.length, MIN_NUMBER_OF_CARD_IN_HAND)

  test("Initial State: the cards that the players have are removed from the DefaultPile"):
    val initPile = setPiles(rand)
    val (hands, newPile) = initPile.giveCardsTo(USER_IDS)

    val givenCards = hands.values.flatten.toList

    assertEquals(givenCards ++ newPile.defaultPile, initPile.defaultPile)

  test("Initial State: the DefaultPile is not empty and the DiscardPile is"):
    assert(initState.cardPiles.defaultPile.nonEmpty)
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

    assertEquals(
      state.play(PickCard(true)).cardPiles,
      state.copy(cardPiles = tookFromDrawPile).cardPiles
    )
    assertEquals(
      state.play(PickCard(false)).cardPiles,
      state.copy(cardPiles = tookFromTrashPile).cardPiles
    )

  test("Event: PlayCard updates the Board with the played card (simple deterministic case)") {
    assume(USER_IDS.length >= 2)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)

    // p1 a 5 Flirt + 1 Flirt => 6 cartes
    val handP1: Hand = Vector.fill(MIN_NUMBER_OF_CARD_IN_HAND)(Flirt) :+ Flirt
    val hands: Map[UserId, Hand] = Map(
      p1 -> handP1,
      p2 -> Vector.empty
    )

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector.empty
    )

    val state = State(
      hands = hands,
      board = board,
      cardPiles = CardPiles(List(Flirt), Nil),
      playerQueue = Queue.from(Seq(p1, p2)),
      log = List.empty
    )

    val result = sm.transition(state)(p1, PlayCard(Flirt, p1))
    val newState = result.getState

    // La carte est posée
    assertEquals(newState.board.getOrElse(p1, Vector.empty), Vector(Flirt))
    // La main de p1 revient à MIN_NUMBER_OF_CARD_IN_HAND
    assertEquals(newState.hands(p1).length, MIN_NUMBER_OF_CARD_IN_HAND)
    // Le tour a tourné vers p2
    assertEquals(newState.playerQueue.head, p2)
  }

  test("Event: Discard add the given card to the trash pile"):
    val cardToPlay = readyToPlayState.getCardToPlay
    val oldTrashPile = readyToPlayState.cardPiles.trashPile
    val newTrashPile = readyToPlayState.play(Discard(cardToPlay)).cardPiles.trashPile

    assertEquals(cardToPlay :: oldTrashPile, newTrashPile)

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
        Study,
        Profession(studyRequired = 1, salary = 4, bonus = None, name = "TestJob")
      )

    val card = Money(3) // amount <= salary (3 <= 4)

    assert(
      card.canBePlaced(playedHand),
      "Money(3) should be placeable when a Profession with salary 4 is present"
    )
  }

  test("Money.canBePlaced: false when no profession is present") {
    val playedHand: PlayedHand =
      Vector(
        Study,
        Study,
        Pet
      )

    val card = Money(2)

    assert(
      !card.canBePlaced(playedHand),
      "Money should not be placeable when no Profession is present"
    )
  }

  test("Money.canBePlaced: false when profession salary is too low") {
    val playedHand: PlayedHand =
      Vector(
        Study,
        Profession(studyRequired = 1, salary = 2, bonus = None, name = "BadJob")
      )

    val card = Money(3) // amount > salary (3 > 2)

    assert(
      !card.canBePlaced(playedHand),
      "Money(3) should NOT be placeable when the only Profession has salary 2"
    )
  }

  test("Profession.canBePlaced: true when enough Study and no existing Profession") {
    val playedHand: PlayedHand =
      Vector(
        Study,
        Study,
        Study
      )

    val card = Profession(studyRequired = 2, salary = 4, bonus = None, name = "NiceJob")

    assert(
      card.canBePlaced(playedHand),
      "Profession(2,4) should be placeable with 3 Study cards and no existing Profession"
    )
  }

  test("Profession.canBePlaced: false when not enough Study") {
    val playedHand: PlayedHand =
      Vector(
        Study // only 1 Study
      )

    val card = Profession(studyRequired = 2, salary = 5, bonus = None, name = "TooHardJob")

    assert(
      !card.canBePlaced(playedHand),
      "Profession(2,5) should NOT be placeable with only 1 Study card"
    )
  }

  test("Profession.canBePlaced: false when there is already a Profession") {
    val playedHand: PlayedHand =
      Vector(
        Study,
        Study,
        Profession(studyRequired = 1, salary = 3, bonus = None, name = "ExistingJob")
      )

    val card = Profession(studyRequired = 2, salary = 6, bonus = None, name = "SecondJob")

    assert(
      !card.canBePlaced(playedHand),
      "A second Profession should NOT be placeable when one Profession is already in the played hand"
    )
  }

  // ## TURNS AND QUEUE TESTS (adaptés à toNextPlayer(State)) ################

  test("toNextPlayer rotates the queue (1,2,3) -> (2,3,1) when no skip-malus present") {
    val state = State(
      hands = Map.empty,
      board = Map.empty,
      cardPiles = CardPiles(Nil, Nil),
      playerQueue = Queue.from(USER_IDS),
      log = List.empty
    )

    val rotatedState = toNextPlayer(state)
    assertEquals(rotatedState.playerQueue.toList, USER_IDS.tail :+ USER_IDS.head)
  }

  test("toNextPlayer on empty queue returns same state with empty queue") {
    val state = State(
      hands = Map.empty,
      board = Map.empty,
      cardPiles = CardPiles(Nil, Nil),
      playerQueue = Queue.empty[UserId],
      log = List.empty
    )

    val rotated = toNextPlayer(state)
    assert(rotated.playerQueue.isEmpty)
  }

  test("isTurnOf is true only for the head of the queue") {
    val q = Queue.from(USER_IDS)
    val head = q.head
    val others = q.tail

    assert(isTurnOf(head, q))

    for user <- others do
      assert(!isTurnOf(user, q))
  }

  // ## COUNTSMILES TESTS  (adaptés à countSmilesMap) ########################

  test("countSmilesMap: basic scoring without malus") {
    val user = USER_IDS.head

    val board: Board = Map(
      user -> Vector(
        Flirt,                                      // +1
        Child,                                      // +2
        Pet,                                        // +1
        Special(Bonus.FreeTravel, "Free Travel")    // +1
      )
    )

    val scores = countSmilesMap(board)

    // Selon SmileValue : 1 + 2 + 1 + 1 = 5
    assertEquals(scores(user), 5)
  }
  

  // SETPILES TESTS ###########################################################

  test("setPiles: generated Money and Profession cards have parameters in expected ranges") {
    val size = 200
    val piles = setPiles(rand, size)

    piles.defaultPile.foreach {
      case Money(amount, _) =>
        assert(
          amount >= 1 && amount <= 4,
          s"Money amount out of range: $amount"
        )

      case Profession(studyRequired, salary, _, _) =>
        // professions définies dans setPiles ont 0 à 6 études, salaires 1 à 4
        assert(
          studyRequired >= 0 && studyRequired <= 6,
          s"Profession.studyRequired out of range: $studyRequired"
        )
        assert(
          salary >= 1 && salary <= 4,
          s"Profession.salary out of range: $salary"
        )

      case _ => // other cards
    }
  }

  // ## END OF GAME TESTS #####################################################

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
    val loser = USER_IDS.tail.head

    val board: Board = Map(
      winner -> Vector(Flirt, Child), // 1 + 2 = 3
      loser -> Vector(Flirt)          // 1
    )

    val state = State(
      hands = Map.empty,
      board = board,
      cardPiles = CardPiles(Nil, Nil), // draw pile empty -> game over
      playerQueue = Queue.from(USER_IDS),
      log = List.empty
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
      playerQueue = Queue.from(USER_IDS),
      log = List.empty
    )

    assertFailure[IllegalMoveException]:
      sm.transition(state)(state.getUserToPlay, PlayCard(Flirt, state.getUserToPlay))
  }

  // ## SKIP TURN TESTS #######################################################

  // Petit helper pour fabriquer un State minimal pour tester toNextPlayer
  def mkState(board: Board, queue: Seq[UserId]): State =
    State(
      hands = Map.empty, // pas utilisé par toNextPlayer
      board = board,
      cardPiles = CardPiles(Nil, Nil), // pas important ici
      playerQueue = Queue.from(queue),
      log = List.empty
    )

  test("SkipTurn: player with Disease skips exactly one turn and malus is destroyed"):
    assume(USER_IDS.length >= 3)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)
    val p3 = USER_IDS(2)

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector(Card.MalusCard(Malus.Disease)), // doit skip
      p3 -> Vector.empty
    )

    val state = mkState(board, Seq(p1, p2, p3))

    // On simule la fin du tour de p1: on passe au suivant
    val next = toNextPlayer(state)

    // p2 doit être sauté, donc le prochain à jouer est p3
    assertEquals(next.playerQueue.head, p3)
    // L'ordre doit rester cyclique
    assertEquals(next.playerQueue.toList, List(p3, p1, p2))
    // Le malus doit être retiré du board de p2
    assertEquals(next.board.getOrElse(p2, Vector.empty), Vector.empty)

  test("SkipTurn: player with Accident also skips one turn"):
    assume(USER_IDS.length >= 3)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)
    val p3 = USER_IDS(2)

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector(Card.MalusCard(Malus.Accident)),
      p3 -> Vector.empty
    )

    val state = mkState(board, Seq(p1, p2, p3))
    val next = toNextPlayer(state)

    // p2 a Accident -> il saute, donc p3 joue
    assertEquals(next.playerQueue.head, p3)
    // Accident détruit
    assertEquals(next.board.getOrElse(p2, Vector.empty), Vector.empty)

  test("SkipTurn: player with BurnOut also skips one turn"):
    assume(USER_IDS.length >= 3)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)
    val p3 = USER_IDS(2)

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector(Card.MalusCard(Malus.BurnOut)),
      p3 -> Vector.empty
    )

    val state = mkState(board, Seq(p1, p2, p3))
    val next = toNextPlayer(state)

    // p2 a BurnOut -> il saute, donc p3 joue
    assertEquals(next.playerQueue.head, p3)
    // BurnOut détruit
    assertEquals(next.board.getOrElse(p2, Vector.empty), Vector.empty)

  test("SkipTurn: if two consecutive players have skip-malus, both skip and rotation continues"):
    assume(USER_IDS.length >= 3)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)
    val p3 = USER_IDS(2)

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector(Card.MalusCard(Malus.Disease)),
      p3 -> Vector(Card.MalusCard(Malus.BurnOut))
    )

    val state = mkState(board, Seq(p1, p2, p3))
    val next = toNextPlayer(state)

    // p2 skip, p3 skip, donc on retombe sur p1
    assertEquals(next.playerQueue.head, p1)
    // Les deux malus doivent être détruits
    assertEquals(next.board.getOrElse(p2, Vector.empty), Vector.empty)
    assertEquals(next.board.getOrElse(p3, Vector.empty), Vector.empty)
    // L'ordre reste cyclique
    assertEquals(next.playerQueue.toList, List(p1, p2, p3))

  test("SkipTurn: skipping stops as soon as we reach a player without skip-malus"):
    assume(USER_IDS.length >= 3)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)
    val p3 = USER_IDS(2)

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector(Card.MalusCard(Malus.Accident)),
      p3 -> Vector.empty
    )

    val state = mkState(board, Seq(p1, p2, p3))
    val next = toNextPlayer(state)

    // p2 skip -> p3 joue
    assertEquals(next.playerQueue.head, p3)
    // p2 a perdu son malus, p3 n'a rien changé
    assertEquals(next.board.getOrElse(p2, Vector.empty), Vector.empty)
    assertEquals(next.board.getOrElse(p3, Vector.empty), Vector.empty)

  // ## TRANSITION TESTS ####################################################

  // Helper avec mains + board + queue
  def mkStateWithHands(
      hands: Map[UserId, Hand],
      board: Board,
      queue: Seq[UserId]
  ): State =
    State(
      hands = hands,
      board = board,
      cardPiles = CardPiles(defaultPile = List(Flirt), trashPile = Nil), // non vide -> pas game over
      playerQueue = Queue.from(queue),
      log = List.empty
    )

  test("transition: playing a normal card on self moves it from hand to board and rotates turn"):
    assume(USER_IDS.length >= 2)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)

    // p1 a exactement MAX cartes en main (5 + 1)
    val handP1: Hand =
      Vector.fill(MIN_NUMBER_OF_CARD_IN_HAND)(Flirt) :+ Flirt
    val handP2: Hand = Vector.empty

    val hands: Map[UserId, Hand] = Map(
      p1 -> handP1,
      p2 -> handP2
    )

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector.empty
    )

    val state = mkStateWithHands(hands, board, Seq(p1, p2))

    val result = sm.transition(state)(p1, PlayCard(Flirt, p1))
    val next = result.getState

    // La carte doit être posée sur le board de p1
    assertEquals(next.board.getOrElse(p1, Vector.empty), Vector(Flirt))
    // La main de p1 doit maintenant contenir MIN cartes
    assertEquals(next.hands(p1).length, MIN_NUMBER_OF_CARD_IN_HAND)
    // Le tour doit avoir tourné vers p2
    assertEquals(next.playerQueue.head, p2)

  test("transition: playing a normal card on another player is illegal"):
    assume(USER_IDS.length >= 2)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)

    val handP1: Hand =
      Vector.fill(MIN_NUMBER_OF_CARD_IN_HAND)(Flirt) :+ Flirt // 6 cartes
    val hands = Map[UserId, Hand](
      p1 -> handP1,
      p2 -> Vector.empty
    )

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector.empty
    )

    val state = mkStateWithHands(hands, board, Seq(p1, p2))

    // Flirt doit être joué sur soi-même, pas sur p2
    assertFailure[IllegalMoveException]:
      sm.transition(state)(p1, PlayCard(Flirt, p2))

  test("transition: playing Disease malus on another player skips their next turn"):
    assume(USER_IDS.length >= 3)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)
    val p3 = USER_IDS(2)

    val disease = Card.MalusCard(Malus.Disease)

    val handP1: Hand =
      Vector.fill(MIN_NUMBER_OF_CARD_IN_HAND)(Flirt) :+ disease // 5 Flirt + 1 Disease
    val hands = Map[UserId, Hand](
      p1 -> handP1,
      p2 -> Vector.empty,
      p3 -> Vector.empty
    )

    val board: Board = Map(
      p1 -> Vector.empty,
      p2 -> Vector.empty,
      p3 -> Vector.empty
    )

    val state = mkStateWithHands(hands, board, Seq(p1, p2, p3))

    val result = sm.transition(state)(p1, PlayCard(disease, p2))
    val next = result.getState

    // p2 devait jouer après p1, mais il a un skip-malus -> c'est p3 qui joue
    assertEquals(next.playerQueue.head, p3)

    // Le malus de skip est consommé (détruit), donc plus sur le board de p2
    assertEquals(next.board.getOrElse(p2, Vector.empty), Vector.empty)

    // p1 a bien joué une carte (sa main revient à MIN cartes)
    assertEquals(next.hands(p1).length, MIN_NUMBER_OF_CARD_IN_HAND)

  test("transition: QuitJob without profession throws IllegalMoveException") {
    assume(USER_IDS.nonEmpty)
    val p1 = USER_IDS(0)

    val hands: Map[UserId, Hand] = Map(
        p1 -> (Vector.fill(MIN_NUMBER_OF_CARD_IN_HAND)(Flirt) :+ Flirt) // 6 cards
    )

    val board: Board = Map(
        p1 -> Vector.empty // no Profession
    )

    val state = State(
        hands = hands,
        board = board,
        cardPiles = CardPiles(defaultPile = List(Flirt), trashPile = Nil), // not game over
        playerQueue = Queue.from(Seq(p1)),
        log = List.empty
    )

    assertFailure[IllegalMoveException] {
        sm.transition(state)(p1, QuitJob)
    }
  }

  test("transition: QuitJob removes profession and rotates turn") {
    assume(USER_IDS.length >= 2)
    val p1 = USER_IDS(0)
    val p2 = USER_IDS(1)

    val job: Card = Profession(
        studyRequired = 1,
        salary = 2,
        bonus = None,
        name = "Test Job"
    )

    val hands: Map[UserId, Hand] = Map(
        p1 -> (Vector.fill(MIN_NUMBER_OF_CARD_IN_HAND)(Flirt)), // 5 cards
        p2 -> Vector.empty
    )

    val board: Board = Map(
        p1 -> Vector(job),
        p2 -> Vector.empty
    )

    val state = State(
        hands = hands,
        board = board,
        cardPiles = CardPiles(defaultPile = List(Flirt), trashPile = Nil), // not game over
        playerQueue = Queue.from(Seq(p1, p2)),
        log = List.empty
    )

    val next = sm.transition(state)(p1, Event.QuitJob).getState

    // Profession removed
    assertEquals(next.board.getOrElse(p1, Vector.empty), Vector.empty)

    // Turn rotated to p2
    assertEquals(next.playerQueue.head, p2)
  }

