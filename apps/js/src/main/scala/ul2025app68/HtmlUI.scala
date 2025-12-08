package apps.ul2025app68
import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.Sequence

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}


@JSExportTopLevel("ul2025app68_Html")
object HtmlUI extends WSClientApp:
  def appId: String = "ul2025app68"
  def uiId: String = "html"
  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    HtmlUIInstance(userId, sendMessage, target)

class HtmlUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends WebClientAppInstance[Event, View](userId, sendMessage, target):
  override val wire = apps.ul2025app68.Wire
  
    override def render(userId: UserId, view: View): Frag =
        view.phaseView match
            case gv: PhaseView.GameView => 
                val fullBoard = createFullBoardParallel(gv)
                val turnMessage: String = if gv.turnOf == userId then "your" else userId + "'s"
                frag(
                    h1("SmileLife"),
                    p("it is " + turnMessage + " turn"),
                    renderPileButtons(userId, gv),
                    renderBoard(userId,gv),
                    renderHand(userId, gv)
                )
            case vv: PhaseView.VictoryView =>
                frag(
                    h1(vv.winners.mkString(" ") + " won!!!!")
                )
    
    
    def renderPileButtons(userId: UserId, view: PhaseView.GameView) =
        val yourTurn = view.turnOf == userId
        div(
            h2("These are the piles"),

            cls := "piles",
            if yourTurn then data.interactive := "interactive" else frag(),
            p(i(s"Pick a card !")),
            div(
                cls := "pile",
                if yourTurn then onclick := { () => sendEvent(Event.PickCard(true)) }
                else frag(),
                p("pile"),
            ),
            div(
                cls := "trash",
                if yourTurn then onclick := { () => sendEvent(Event.PickCard(false)) }
                else frag(),
                p("trash : " + view.lastDiscard.toString)
            )
        )

    def renderBoard(userId: UserId, view: PhaseView.GameView) =
        val l = for{
            player: UserId <- view.board.keySet
            playedHand = view.board(player)
            cards = playedHand.map(card => card.toString)

            title = p("This is ", span(player), "'s placed cards")
            space = p()
            hand = p("(" + cards.mkString(", ") + ")\n")
            d = div(title,hand,space)
        } yield d

        div(h2("board"),
        l.toSeq)

    def renderHand(userId: UserId, view: PhaseView.GameView) =
        val hand = view.hand.toSeq
        val stringHand = hand.map(card => card.toString)
        val dropDownElem = select(
            stringHand.map(card => option(value := card, card))*
        ).render
        div(
            h2("this is your hand"),
            p("(" + stringHand.mkString(", ") + ")"),
            dropDownElem,
            button(
                cls := "action",
                "Play Card",
                onclick := { () => 
                    val chosenCard = dropDownElem.value       // get selected value
                    val cardIndex = stringHand.indexOf(chosenCard)
                    val card = hand(cardIndex)                // get original card object
                    sendEvent(Event.PlayCard(card))           // send event
                }
            ),
            button(
                cls := "action",
                "Discard Card",
                onclick := { () => 
                    val chosenCard = dropDownElem.value       // get selected value
                    val cardIndex = stringHand.indexOf(chosenCard)
                    val card = hand(cardIndex)                // get original card object
                    sendEvent(Event.Discard(card))           // send event
                }
            )
        )

    def renderCardDropdown(cards: Seq[String]) =
        select(
        cards.map(card => option(value := card, card))*
        ).render

    def createFullBoardParallel(view: PhaseView.GameView): Future[FullBoard] = {
    val futures: Seq[Future[(UserId, PlayerBoard)]] = view.board.keySet.toSeq.map { userId =>
        Future {
        val board = view.board(userId)
        val playerBoard = PlayerBoard(
            board.count { case Card.Flirt => true; case _ => false },
            board.count { case Card.Child => true; case _ => false },
            board.filter { case Card.Money => true; case _ => false },
            board.find { case Card.Profession => true; case _ => false },
            board.count { case Card.Study => true; case _ => false },
            board.count { case Card.Pet => true; case _ => false },
            board.filter { case Card.Malus => true; case _ => false },
            board.filter { case Card.Special => true; case _ => false }
        )
        userId -> playerBoard
        }
    }

    Future.sequence(futures).map(seq => seq.toMap)
    }