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
import scala.caps.use

/**
 * HTML UI for the SmileLife game.
 * Provides the main entry point for the web-based game interface.
 */
@JSExportTopLevel("ul2025app68_Html")
object HtmlUI extends WSClientApp:
  def appId: String = "ul2025app68"
  def uiId: String = "html"
  
  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    HtmlUIInstance(userId, sendMessage, target)

/**
 * Main UI instance for rendering the SmileLife game.
 * Handles all game views including board state, hand, and end screen.
 */
class HtmlUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends WebClientAppInstance[Event, View](userId, sendMessage, target):
  
  override val wire = apps.ul2025app68.Wire
  
  override def render(userId: UserId, view: View): Frag =
    view.phaseView match
      case gv: PhaseView.GameView => 
        val fullBoard = createFullBoardParallel(gv)
        val turnMessage: String = if gv.turnOf == userId then "your" else gv.turnOf + "'s"
        frag(
          div(
            cls := "section",
            h1("SmileLife"),
            renderKillGame,
            renderLogs(gv.log),
            p("it is " + turnMessage + " turn")
          ),
          renderPileButtons(userId, gv),
          renderBoard(fullBoard),
          renderHand(userId, gv)
        )
      case vv: PhaseView.VictoryView => renderEndScreen(vv, userId)
  
  /** Renders the button to end/kill the game */
  private def renderKillGame: Frag =
    button(
      cls := "kill",
      "Kill Game",
      onclick := { () => sendEvent(Event.EndGame) }
    )
  
  /** Renders expandable log section showing game history */
  private def renderLogs(logs: Seq[String]): Frag =
    details(
      summary("Game Logs"),
      ul(logs.map(log => li(log))*)
    )
  
  /** Renders pile selection buttons (pile and trash) */
  private def renderPileButtons(userId: UserId, view: PhaseView.GameView): Frag =
    val yourTurn = view.turnOf == userId
    div(
      cls := "section",
      h2("These are the piles"),
      p(i("Pick a card!")),
      div(
        cls := "piles",
        if yourTurn then data.interactive := "interactive" else frag(),
        button(
          cls := "pile",
          if yourTurn then onclick := { () => sendEvent(Event.PickCard(true)) }
          else frag(),
          p("Pile: " + view.defaultPileSize + " Cards Left")
        ),
        button(
          cls := "trash",
          if yourTurn then onclick := { () => sendEvent(Event.PickCard(false)) }
          else frag(),
          p("Trash: " + view.lastDiscard.fold("No Cards in Trash")(cardName))
        )
      )
    )
  
  /** Renders all player boards */
  private def renderBoard(boards: FullBoard): Frag =
    val boardDivs = for {
      (userId, board) <- boards
    } yield renderPlayerBoard(userId, board)

    div(
      cls := "boards",
      h2("Board"),
      boardDivs.toSeq,
      button(
        cls := "action",
        "Quit Job",
        onclick := { () => sendEvent(Event.QuitJob) }
      )
    )

  /** Renders a single player's board state */
  private def renderPlayerBoard(userId: UserId, board: PlayerBoard): Frag =
    def getStatus(board: PlayerBoard): String =
      board.marriage match
        case 0 => "Single"
        case 1 => "Married"
        case 2 => "Married Twice?!"
        case _ => "Error: Invalid Marriage State"

    div(
      cls := "playerBoard",
      div(
        cls := "topBoard",
        p("These are " + userId + "'s cards"),
        p(board.smiles.toString + " 🙂")
      ),
      div(
        cls := "statsRow",
        p("Flirts: ", board.flirt.toString),
        p("Child: ", board.child.toString),
        p("Study: ", board.study.toString),
        p("Pet: ", board.pet.toString),
        p("Travels: ", board.travels.toString),
        p("Status: ", getStatus(board))
      ),
      div(
        cls := "professionSection",
        board.profession match
          case Some(profession: Profession) => cardDiv(profession)
          case _ => p("Unemployed")
      ),
      div(
        cls := "cardGroups",
        renderExpandable("Money", board.money.collect { case Card.Money(a, false) => "Salary: " + a.toString }),
        renderExpandable("Malus", board.malus.map(baseMalusName)),
        renderExpandable("Special", board.special.map(_.name)),
        renderExpandable("Houses", board.houses.map(cardName))
      )
    )

  /** Renders an expandable section for card lists */
  private def renderExpandable(title: String, cards: Seq[String]): Frag =
    if cards.isEmpty then
      p(s"No $title cards")
    else
      details(
        summary(s"$title (${cards.size})"),
        ul(cards.map(c => li(c))*)
      )
  
  /** Renders the player's hand with card selection and play options */
  private def renderHand(userId: UserId, view: PhaseView.GameView): Frag =

    val hand = view.hand.toSeq
    val stringHand = hand.map(card => cardName(card))
    val dropDownHand = select(
      stringHand.map(card => option(value := card, card))*
    ).render
    val otherPlayers = view.board.keySet.toSeq.filterNot(_ == userId)
    val dropDownChoice: org.scalajs.dom.HTMLSelectElement = select(
      otherPlayers.map(u => option(value := u, u))*
    ).render
    
    var choiceHolder: org.scalajs.dom.Element = div(p("no selected card")).render
    var selectedReceiver: UserId = userId

    def updateReceiverForCard(): Unit = {
      if hand.nonEmpty then
        val selectedCard = hand(stringHand.indexOf(dropDownHand.value))
        selectedCard match
          case _: Card.MalusCard =>
            choiceHolder.textContent = ""
            choiceHolder.appendChild(dropDownChoice)
            selectedReceiver = if otherPlayers.nonEmpty then otherPlayers.head else userId
          case _ =>
            choiceHolder.textContent = "you"
            selectedReceiver = userId
    }

    updateReceiverForCard()

    dropDownChoice.addEventListener("change", _ => {
      selectedReceiver = dropDownChoice.value
    })
    
    dropDownHand.addEventListener("change", _ => {
      updateReceiverForCard()
    })

    div(
      cls := "section",
      h2("This is your hand"),
      div(
        cls := "flex-row",
        hand.map(item => cardDiv(item))
      ),
      dropDownHand,
      button(
        cls := "action",
        "Play Card",
        onclick := { () =>
          val chosenCard = dropDownHand.value
          val cardIndex = stringHand.indexOf(chosenCard)
          val card = hand(cardIndex)
          sendEvent(Event.PlayCard(card, selectedReceiver))
        }
      ),
      button(
        cls := "action",
        "Discard Card",
        onclick := { () =>
          val chosenCard = dropDownHand.value
          val cardIndex = stringHand.indexOf(chosenCard)
          val card = hand(cardIndex)
          sendEvent(Event.Discard(card))
        }
      ),
      div(
        p("Receiver: "),
        choiceHolder
      )
    )
  
  /** Renders the victory/loss screen */
  private def renderEndScreen(view: PhaseView.VictoryView, userId: UserId): Frag =
    val smilesList = view.board.map((id,_) => id -> view.board(id).map(card => card.smileValue).sum)
    div(
        if view.winners.contains(userId) then
        div(
            cls := "section",
            h1("Congratulations, YOU WON!"),
            h2("Player(s) " + view.winners.mkString(", ") + " won! With " + view.board(view.winners.head).map(card => card.smileValue).sum + " Smiles.")
        )
        else
        div(
            cls := "section",
            h1("You... Lost..."),
            h2("Player(s) " + view.winners.mkString(", ") + " won! With " + view.board(view.winners.head).map(card => card.smileValue).sum + " Smiles.")
        )
        ,
        div(
            div( 
                (for {
                    (userId) <- view.board.keySet
                } yield p(userId + ": " + smilesList(userId) + " smiles.")).toSeq
            )
        )
    )
    
  
  /** Transforms game view board into a structured FullBoard representation */
  private def createFullBoardParallel(view: PhaseView.GameView): FullBoard =
    view.board.map { (userId, board) =>
      userId ->
      PlayerBoard(
        board.count { case Card.Flirt => true; case _ => false },
        board.count { case Card.Child => true; case _ => false },
        board.collect { case m: Card.Money => m },
        board.collectFirst { case p: Card.Profession => p },
        board.count { case Card.Study => true; case _ => false },
        board.count { case Card.Pet => true; case _ => false },
        board.collect { case Card.MalusCard(malus) => malus },
        board.collect { case s: Card.Special => s },
        board.collect { case s: Card.House => s },
        board.count { case Card.Travel(_) => true; case _ => false },
        board.count { case Card.Marriage => true; case _ => false },
        view.board(userId).map(card => card.smileValue).sum
      )
    }
  
  /** Converts a card to its display string */
  private def cardName(card: Card): String =
    card match
      case Card.Flirt | Card.Marriage | Card.Child | Card.Study | Card.Pet =>
        card.toString
      case Card.MalusCard(malus) =>
        malus.toString
      case Card.House(price) =>
        "House: " + price + "$"
      case Card.Travel(price) =>
        "Travel: " + price + "$"
      case Card.Special(bonus, name) =>
        name
      case Card.Money(amount, false) =>
        "Money: " + amount
      case Card.Money(amount, true) =>
        "Used Money: " + amount
      case Card.Profession(studyRequired, salary, _, name) =>
        name + " " + studyRequired + "🎓, " + salary + "💰"
  
  /** Converts a bonus to its display string */
  private def bonusName(bonus: Bonus): String =
    bonus match
      case Bonus.MalusProtection(mal) =>
        malusName(mal) + " resistant"
      case Bonus.FreeHouse =>
        "🏠 Free Houses"
      case Bonus.FreeTravel =>
        "✈️ Free Travels"
      case Bonus.UnlimitedFlirt =>
        "💘 Unlimited Flirts"
      case Bonus.FlirtWhileMarried =>
        "💔💘 Cheating Possible"
      case Bonus.UnlimitedStudy =>
        "🎓 Unlimited Study"
      case Bonus.StudyWhileWorking =>
        "📚 Study while Employed"
      case Bonus.DoubleMarriage =>
        "💘 Can Marry Twice"
      case Bonus.DoubleStudy =>
        "📚 Double Study Level"
  
  /** Converts a malus to its display string with emoji */
  private def malusName(malus: Malus): String =
    val emoji = malus match
      case Malus.Disease =>
        "🦠 "
      case Malus.Accident =>
        "🚑 "
      case Malus.BurnOut =>
        "🔥 "
      case Malus.Tax =>
        "💸 "
      case Malus.Divorce =>
        "💔 "
      case Malus.Dismissal =>
        "📤 "
      case Malus.TerroristAttack =>
        "💣 "
      case Malus.RepeatYear =>
        "🔄 "
    emoji + baseMalusName(malus)

/** Converts a malus to its display string */
  def baseMalusName(malus: Malus): String =
    val emoji = malus match
      case Malus.Disease =>
        "Disease"
      case Malus.Accident =>
        "Accident"
      case Malus.BurnOut =>
        "Burn-Out"
      case Malus.Tax =>
        "Tax"
      case Malus.Divorce =>
        "Divorce"
      case Malus.Dismissal =>
        "Dismissal"
      case Malus.TerroristAttack =>
        "Terrorist Attack"
      case Malus.RepeatYear =>
        "Repeat Year"
    malus.toString
  
  /** Renders a card as an HTML div element */
  private def cardDiv(card: Card): HTMLDivElement =
    val smiles = card.smileValue.toString + " 🙂"
    
    card match
      case Card.Flirt | Card.Marriage | Card.Child | Card.Study | Card.Pet =>
        div(
          cls := "square",
          div(cls := "top", ""),
          div(cls := "middle", card.toString),
          div(cls := "bottom", smiles)
        ).render
      
      case Card.MalusCard(malus) =>
        div(
          cls := "square",
          div(cls := "top", "Malus"),
          div(cls := "middle", malusName(malus)),
          div(cls := "bottom", "")
        ).render
      
      case Card.House(price) =>
        div(
          cls := "square",
          div(cls := "top", ""),
          div(cls := "middle", "🏠 House: " + price + "$"),
          div(cls := "bottom", smiles)
        ).render
      
      case Card.Travel(price) =>
        div(
          cls := "square",
          div(cls := "top", ""),
          div(cls := "middle", "✈️ Travel: " + price + "$"),
          div(cls := "bottom", smiles)
        ).render
      
      case Card.Special(bonus, name) =>
        div(
          cls := "square",
          div(cls := "top", name),
          div(cls := "middle", bonusName(bonus)),
          div(cls := "bottom", "")
        ).render
      
      case Card.Money(amount, _) =>
        div(
          cls := "square",
          div(cls := "top", ""),
          div(cls := "middle", "💸 Money: " + amount + "$"),
          div(cls := "bottom", smiles)
        ).render
      
      case Card.Profession(studyRequired, salary, bonus, name) =>
        div(
          cls := "square",
          div(cls := "top", s"🎓${studyRequired} | 💰${salary}"),
          div(cls := "middle", name),
          div(cls := "bottom", 
            bonus match
              case Some(profs) =>
                profs.map(b => bonusName(b)).mkString(" | ") + " | " + smiles
              case None =>
                smiles
          )
        ).render
  
  override def css: String = super.css + """
    html {
      font-family: sans-serif;
      background: #add8e6;
    }

    .playerBoard {
      display: flex;
      flex-direction: column;
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
      width: 50%;
      min-height: 50px;
      margin: auto;
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

    .topBoard {
      display: flex;
      justify-content: space-between;
      align-items: center;
      width: 100%;
    }

    .kill {
      padding: 4px 8px;
      font-size: 12px;
      background-color: #5b7c87ff;
      color: white;
      border: none;
      border-radius: 5px;
      cursor: pointer;
      transition: background-color 0.15s ease;
    }

    .kill:hover {
      background-color: #306aa8ff;
    }

    .kill:active {
      background-color: #1919a0ff;
      transform: scale(0.97);
    }

    .action {
      padding: 8px 16px;
      font-size: 14px;
      background-color: #5b7c87ff;
      color: white;
      border: none;
      border-radius: 5px;
      cursor: pointer;
      transition: background-color 0.15s ease;
    }

    .action:hover {
      background-color: #306aa8ff;
    }

    .action:active {
      background-color: #1919a0ff;
      transform: scale(0.97);
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

    details > ul > li:last-child {
      border-bottom: none;
    }

    h1 + p {
      margin-top: 0.1;
      margin-bottom: 0.1;
    }
  """