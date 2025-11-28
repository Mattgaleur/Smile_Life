package apps.ul2025app68

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

import scala.util.Random
import cs214.webapp.server.StateMachine

class LogicTests extends WebappSuite[Event, State, View]:

    val sm = Logic()

    // ## INITIAL STATE TESTS
    test("Initial State: all player have the right number of card"):
        ???
    
    test("Initial State: the cards that the player have are removed from the DefaultPile"):
        ???

    test("Initial State: the DefaultPile is not empty and the DiscardPile is"):
        ???


    // ## EVENT TESTS
    test("Event: PickCard remove card from the relevant Pile"):
        ???

    test("Event: PlayCard updates the Board with the played card"):
        ???

    test("Event: Discard add the given card to the DiscardPile"):
        ???

    test("Event: The player shouldn't be able to play when it ain't his turn"):
        ???

    test("Event: The player shouldn't be able to PickCard from an empty Pile"):
        ???


    // ## END OF GAME TESTS

    // I don't really know how the games ends so I let it to you