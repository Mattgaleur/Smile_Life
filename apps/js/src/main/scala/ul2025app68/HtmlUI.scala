package apps.ul2025app68


import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel

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
    frag(
        html(
            head(
                script("SmileLife")
            ),
            body(
                h1("This is the piles"),
                div(
                p("This is my first paragraph"),
                p("This is my second paragraph")
                )
            )
        )
    )

    def renderPileButtons(userId: UserId, view: View) =
        val yourTurn = false
        p(
        p(i(s"Pick a card !")),
        cls := "piles",
        if yourTurn then data.interactive := "interactive" else frag(),
        div(
            cls := "pile",
            if yourTurn then onclick := { () => sendEvent(Event.PickCard(true)) }
            else frag(),
            p("emoji"),
        ),
        div(
            cls := "trash",
            if yourTurn then onclick := { () => sendEvent(Event.PickCard(false)) }
            else frag(),
            p("emoji")
        )
        )


