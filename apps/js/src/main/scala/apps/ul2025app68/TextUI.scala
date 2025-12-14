package apps.apps.ul2025app68

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("ul2025app68_Text")
object TextUI extends WSClientApp:
  def appId: String = "ul2025app68"
  def uiId: String = "text"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    TextUIInstance(userId, sendMessage, target)


class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target) 
    extends graphics.TextClientAppInstance[Event, View](userId, sendMessage, target):
    
    override val wire = apps.ul2025app68.Wire

    override def renderView(userId: UserId, view: View): Vector[TextSegment] =
        view.phaseView match
            case gv: PhaseView.GameView => 
                trashView(gv) ++ turnView(userId, gv) ++ boardView(gv) ++ handView(userId,gv)
            case vv: PhaseView.VictoryView => victoryView(vv)
        
    def trashView(view: PhaseView.GameView): Vector[TextSegment] = 
        if view.lastDiscard == Card.Special then Vector(TextSegment("There are not cards in the trash."))
        //need to change later
            else Vector(TextSegment("Last trashed card was : " + view.lastDiscard.toString),
            TextSegment("\n"))
    
    def turnView(userId: UserId, view: PhaseView.GameView): Vector[TextSegment] =
        if view.turnOf == userId then Vector(TextSegment("This is your turn."),
            TextSegment("\n"))
            else Vector(TextSegment("This is " + view.turnOf + "'s turn."),
                TextSegment("\n"))
    
    def boardView(view: PhaseView.GameView): Vector[TextSegment] =
        val l = for{
            player: UserId <- view.board.keySet
            playedHand = view.board(player)
            cards = playedHand.map(card => card.toString)

            title = TextSegment("This is " + player + "'s placed cards")
            space = TextSegment("\n")
            hand = TextSegment("(" + cards.mkString(", ") + ")\n")
            vector = Vector(title,space,hand,space,space)
        } yield vector

        l.flatten.toVector
    
    def handView(player: UserId, view: PhaseView.GameView): Vector[TextSegment] =
        Vector(
            TextSegment("this is your hand\n"),
            TextSegment("\n"),
            TextSegment("(" + view.hand.map(card => card.toString).mkString(", ") + ")"),
            TextSegment("\n\n")
        )
    
    def victoryView(view: PhaseView.VictoryView): Vector[TextSegment] =
        Vector(
            TextSegment(view.winners.mkString(" ") + " won!!!!")
        )

    override def handleTextInput(view: View, text: String): Option[Event] =
        val input: List[String] = text.split(" ").toList
        view.phaseView match
            case gv: PhaseView.GameView => 
                input(0) match
                    case "draw" => input(1) match
                        case "pile" => Option(Event.PickCard(true))
                        case "trash" => Option(Event.PickCard(false))
                        case _ => throw new Exception("unknown command")
            
                    case "play" => if 1 <= input(1).toInt && input(1).toInt <= 6 then Option(Event.PlayCard(gv.hand(input(1).toInt-1), userId))
                        else throw new Exception("unknown card index")
                    case "discard" => if 1 <= input(1).toInt && input(1).toInt <= 6 then Option(Event.Discard(gv.hand(input(1).toInt-1)))
                        else throw new Exception("unknown card index")

                    case _ => throw new Exception("unknown command")
            case vv: PhaseView.VictoryView => None
        
    
    override def css: String = super.css + """
        html {
            font-family: sans-serif;
            background: #add8e6;
        }
        """