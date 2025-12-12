package apps.ul2025app68
import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.{details, summary}
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.Sequence
import org.scalajs.dom.HTMLDivElement
import apps.ul2025app68.Card.Profession


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
                    p("trash : " + view.lastDiscard.fold("No Cards in Trash")(cardName))
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
            board.profession match
                case Some(profession: Profession) => frag(cardDiv(profession))
                case _ => p("Unemployed")
            ),

            // 3️⃣ Expandable card groups in one line
            div(
            cls := "cardGroups",
            renderExpandable("Money", board.money.collect{case Card.Money(a,false) => "Salary : " + a.toString}),
            renderExpandable("Malus", board.malus.map(_.toString)),
            renderExpandable("Special", board.special.map(_.name))
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
        val stringHand = hand.map(card => cardName(card))
        val dropDownHand = select(
            stringHand.map(card => option(value := card, card))*
        ).render
        val dropDownChoice: org.scalajs.dom.HTMLSelectElement = select(
                view.board.keySet.toSeq.map(userId => option(value := userId, userId))*
        ).render
        var choiceHolder: org.scalajs.dom.Element = div(p("no selected card")).render
        
        dropDownHand.addEventListener("change", _ => {
            val selectedCard = hand(stringHand.indexOf(dropDownHand.value))
            selectedCard match
                case Card.MalusCard(_) => choiceHolder.textContent = ""
                    choiceHolder.appendChild(dropDownChoice)
                case _ => choiceHolder.textContent = "you"
            })

        

        div(
            cls:= "section",
            h2("this is your hand"),
            //p("(" + stringHand.mkString(", ") + ")"),
            div(
                cls := "flex-row",
                hand.map(item => cardDiv(item))
            ),
            dropDownHand,
            button(
                cls := "action",
                "Play Card",
                onclick := { () => 
                    val chosenCard = dropDownHand.value       // get selected value
                    val cardIndex = stringHand.indexOf(chosenCard)
                    val card = hand(cardIndex)                // get original card object
                    val receiver = choiceHolder match
                        case e: org.scalajs.dom.HTMLSelectElement => e.value
                        case _ => userId
                    
                    sendEvent(Event.PlayCard(card,receiver))           // send event
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
                board.collect { case m: Card.Money => m},
                board.collectFirst { case p: Card.Profession => p},
                board.count { case Card.Study => true; case _ => false },
                board.count { case Card.Pet => true; case _ => false },
                board.collect { case Card.MalusCard(malus) => malus},
                board.collect { case s: Card.Special => s}
            )
        }
    
    def cardName(card: Card): String =
        card match
            case Card.Flirt | Card.Marriage | Card.Child | Card.Study | Card.Pet => card.toString
            case Card.MalusCard(malus) => malus.toString
            case Card.House(price) => "House : " + price + "$"
            case Card.Travel(price) => "Travel : " + price + "$"
            case Card.Special(bonus, name) => name
            case Card.Money(amount, false) => "Money: " + amount
            case Card.Money(amount, true) => "Used Money: " + amount
            case Card.Profession(studyRequired, salary, Some(bonus), name) => 
                name + " " + studyRequired + "🎓, " + salary + "💰, " + bonus.mkString(", ")
            case Card.Profession(studyRequired, salary, None, name) =>
                name + " " + studyRequired + "🎓, " + salary + "💰"
    
    def bonusName(bonus: Bonus): String = 
        bonus match
            case Bonus.MalusProtection(mal: Malus) => malusName(mal) + " resistant"
            case Bonus.FreeHouse          => "🏠 One Free House"
            case Bonus.FreeTravel         => "✈️ Free Travels"
            case Bonus.UnlimitedFlirt     => "💘 Unlimited Flirts"
            case Bonus.FlirtWhileMarried  => "💔💘 Cheating Possible"
            case Bonus.UnlimitedStudy     => "🎓 Unlimited Study"
            case Bonus.StudyWhileWorking  => "📚 Study while Employed"
        
    
    def malusName(malus: Malus): String =
        val emoji = malus match
            case Malus.Disease         => "🦠 "
            case Malus.Accident        => "🚑 "
            case Malus.BurnOut         => "🔥 "
            case Malus.Tax             => "💸 "
            case Malus.Divorce         => "💔 "
            case Malus.Dismissal       => "📤 "
            case Malus.TerroristAttack => "💣 "
            case Malus.RepeatYear      => "🔄 "
        emoji + malus.toString
    
    def cardDiv(card: Card): HTMLDivElement =    
        card match
            case Card.Flirt | Card.Marriage | Card.Child | Card.Study | Card.Pet => 
                div(
                    cls := "square",
                    div(cls := "middle", card.toString)
                ).render
            case Card.MalusCard(malus) =>
                div(
                    cls := "square",
                    div(cls := "middle", malusName(malus))
                ).render
            case Card.House(price) => 
                div(
                    cls := "square",
                    div(cls := "middle", "🏠 House : " + price + "$")
                ).render
            case Card.Travel(price) =>
                div(
                    cls := "square",
                    div(cls := "middle", "✈️ Travel : " + price + "$")
                ).render
            case Card.Special(bonus, name) =>
                div(
                    cls := "square",
                    div(cls := "top", "Special"),
                    div(cls := "middle", bonusName(bonus))
                ).render
            case Card.Money(amount, used) =>
                div(
                    cls := "square",
                    div(cls := "middle", "💸 Money: " + amount + "$")
                ).render
            case Card.Profession(studyRequired, salary, bonus, name) =>
                div(
                    cls := "square",
                    div(cls := "top", s"🎓${studyRequired} | 💰${salary}"),
                    div(cls := "middle", name),
                    div(cls := "bottom", bonus match 
                        case Some(prof) => prof.map(b => bonusName(b)).mkString(" | ")
                        case _ => "You're Useless"
                    )
                ).render
            
        
                
        
    
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

        .flex-row {
            display: flex;
            gap: 20px;
            justify-content: center;
            flex-wrap: nowrap;
        }

        .square {
            width: 120px;
            height: 150px;
            background: #91b7c4;
            border: 2px solid #5b7c87;
            border-radius: 8px;
            display: flex;
            flex-direction: column;
            justify-content: space-between;
            align-items: center;
            padding: 5px;
            text-align: center;
            font-size: 0.9rem;
        }

        .top {
            font-weight: bold;
        }

        .middle {
            font-size: 1rem;
        }

        .bottom {
            font-size: 0.8rem;
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