package apps.ul2025app68

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target) 
    extends graphics.TextClientAppInstance[Event, View](userId, sendMessage, target):
    
    override val wire = apps.ul2025app68.Wire

    val commands = Map(
    "draw" -> ???,
    "play" -> ???,
    "discard" -> ???
    )
    
    val drawCommands = Map(
        "pile" -> ???,
        "trash" -> ???
    )

    override def renderView(userId: UserId, view: View): Vector[TextSegment] =

        boardView(view) ++ handView(userId,view)
    
    def boardView(view: View): Vector[TextSegment] =
        val l = for{
            player: UserId <- view.board.keySet
            playedHand = view.board(player)
            cards = playedHand.map(card => card.toString)

            title = TextSegment("This is " + player + "'s cards")
            space = TextSegment("\n")
            hand = TextSegment("(" + cards.mkString(", ") + ")")
            vector = Vector(title,space,hand,space,space)
        } yield vector

        l.flatten.toVector
    
    def handView(player: UserId, view: View): Vector[TextSegment] =
        Vector(
            TextSegment("this is your hand"),
            TextSegment("\n"),
            TextSegment("(" + view.hand.map(card => card.toString).mkString(", ") + ")"),
            TextSegment("\n\n")
        )

    override def handleTextInput(view: View, text: String): Option[Event] =
        val input = text.split(" ").toList
        ???
            