package apps.ul2025app68
import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.{details, summary}
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.Sequence
import org.scalajs.dom.HTMLDivElement


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
                    div(
                        cls:= "section",
                        h1("SmileLife"),
                        p("it is " + turnMessage + " turn")
                    ),
                    renderPileButtons(userId, gv),
                    renderBoard(fullBoard),
                    renderHand(userId, gv)
                )
            case vv: PhaseView.VictoryView =>
                frag(
                    h1(vv.winners.mkString(" ") + " won!!!!")
                )
    
    
    def renderPileButtons(userId: UserId, view: PhaseView.GameView) =
        val yourTurn = view.turnOf == userId
        div( cls:= "section",
            h2("These are the piles"),
            p(i(s"Pick a card !")),
            
            div(
                cls := "piles",
                if yourTurn then data.interactive := "interactive" else frag(),
                button(
                    cls := "pile",
                    if yourTurn then onclick := { () => sendEvent(Event.PickCard(true)) }
                    else frag(),
                    p("pile"),
                ),
                button(
                    cls := "trash",
                    if yourTurn then onclick := { () => sendEvent(Event.PickCard(false)) }
                    else frag(),
                    p("trash : " + view.lastDiscard.toString)
                )
            )
        )

    def renderBoard(boards: FullBoard) =
        val l = for{
            (userId, board) <- boards
        } yield renderPlayerBoard(userId, board)

        div( cls:= "boards",
            h2("board"),
            l.toSeq
        )

    def renderPlayerBoard(userId: UserId, board: PlayerBoard) =
        div(
            p("These are " + userId + "'s cards"),
            cls := "playerBoard",

            // 1️⃣ Stats line
            div(
            cls := "statsRow",
            p("Flirts: ", board.flirt.toString),
            p("Child: ", board.child.toString),
            p("Study: ", board.study.toString),
            p("Pet: ", board.pet.toString)
            ),

            // 2️⃣ Profession
            div(
            cls := "professionSection",
            p(board.profession.map(_.toString).getOrElse("Unemployed"))
            ),

            // 3️⃣ Expandable card groups in one line
            div(
            cls := "cardGroups",
            renderExpandable("Money", board.money.map(_.toString)),
            renderExpandable("Malus", board.malus.map(_.toString)),
            renderExpandable("Special", board.special.map(_.toString))
            )
        )

    def renderExpandable(title: String, cards: Seq[String]): Frag =
    if cards.isEmpty then
        p(s"No $title cards")
    else
        details(
        summary(s"$title (${cards.size})"),
        ul(cards.map(c => li(c))*)
        )

    def renderHand(userId: UserId, view: PhaseView.GameView) =
        val hand = view.hand.toSeq
        val stringHand = hand.map(card => card.toString)
        val dropDownHand = select(
            stringHand.map(card => option(value := card, card))*
        ).render
        val dropDownChoice = select(
                view.board.keySet.toSeq.map(userId => option(value := userId, userId))*
        ).render
        var choiceHolder: HTMLDivElement = div(p("no selected card")).render
        
        dropDownHand.addEventListener("change", _ => {
            val selectedCard = hand(stringHand.indexOf(dropDownHand.value))
            selectedCard match
                case Card.Malus => choiceHolder.textContent = ""
                    choiceHolder.appendChild(dropDownChoice)
                case _ => choiceHolder.textContent = "you"
            })

        

        div(
            cls:= "section",
            h2("this is your hand"),
            p("(" + stringHand.mkString(", ") + ")"),
            dropDownHand,
            button(
                cls := "action",
                "Play Card",
                onclick := { () => 
                    val chosenCard = dropDownHand.value       // get selected value
                    val cardIndex = stringHand.indexOf(chosenCard)
                    val card = hand(cardIndex)                // get original card object
                    sendEvent(Event.PlayCard(card))           // send event
                }
            ),
            button(
                cls := "action",
                "Discard Card",
                onclick := { () => 
                    val chosenCard = dropDownHand.value       // get selected value
                    val cardIndex = stringHand.indexOf(chosenCard)
                    val card = hand(cardIndex)                // get original card object
                    sendEvent(Event.Discard(card))           // send event
                }
            ),
            div(p("Receiver : "),
                choiceHolder
            )
        )
        
    
    def createFullBoardParallel(view: PhaseView.GameView): FullBoard = 
        view.board.map { (userId, board) =>
            userId ->
            PlayerBoard(
                board.count { case Card.Flirt => true; case _ => false },
                board.count { case Card.Child => true; case _ => false },
                board.filter { case Card.Money => true; case _ => false },
                board.find { case Card.Profession => true; case _ => false },
                board.count { case Card.Study => true; case _ => false },
                board.count { case Card.Pet => true; case _ => false },
                board.filter { case Card.Malus => true; case _ => false },
                board.filter { case Card.Special => true; case _ => false }
            )
        }
    
    override def css: String = super.css + """
        html {
            font-family: sans-serif;
            background: #add8e6;
        }

        .playerBoard {
            display: flex;
            flex-direction: column; /* vertical stacking */
            align-items: flex-start;
            gap: 2px;
            border: 1px solid #5b7c87ff;
            border-radius: 8px;
            padding: 5px;
            background-color: #91b7c4ff;
            margin-bottom: 5px;
        }

        .hands, .statsRow {
            display: flex;
            flex-direction: row;
            justify-content: center;
            gap: 15px;
            align-items: center;
            margin: auto;
        }

        .professionSection {
            border-radius: 5px;
            background-color: #91b7c4ff;
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            text-align: center;
            width: 150px;
            min-height: 50px;
            margin: auto;
            width: 50%;
            border: 3px solid #5b7c87ff;
            padding: 10px;
        }

        .cardGroups {
            display: flex;
            flex-direction: row;
            gap: 10px;
            text-align: center;
            justify-content: center;
            margin: auto;
        }

        .section, .boards, .piles {
            display: flex;
            flex-direction: column;
            margin: auto;
            justify-content: center;
            text-align: center;
            width: 100%;
            gap: 2px;
            line-height: 1;
        }

        .boards {
            gap: 15px;
        }

        .piles {
            display: grid;
            grid-template-columns: 1fr 1fr;
        }

        details > ul {
            padding: 5px;
            margin: 0;
            border: 1px solid #5b7c87ff;
            border-radius: 5px;
            background-color: #91b7c4ff;
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
        }

        details > ul > li {
            padding: 3px;
            margin: 2px 0;
            border-bottom: 1px solid #5b7c87ff;
        }

        h1 + p {
            margin-top: 0.1;
            margin-bottom: 0.1;
        }
        """