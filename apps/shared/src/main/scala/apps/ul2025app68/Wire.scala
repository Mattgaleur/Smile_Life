package apps
package ul2025app68

import cs214.webapp.*
import cs214.webapp.DecodingException
import Event.*
import View.*
import ujson.*

import scala.util.{Failure, Success, Try}
import ul2025app68.PhaseView.*
import ul2025app68.Card.*


object Wire extends AppWire[Event, View]:

    override object eventFormat extends WireFormat[Event]:
        override def encode(event: Event): Value =
            event match
                case Discard(card) => 
                    Obj("type" -> "Discard", "card" -> CardWire.encode(card))
                case PlayCard(card) =>
                    Obj("type" -> "PlayCard", "card" -> CardWire.encode(card))
                case PickCard(isDefaultPile) =>
                    Obj(
                        "type" -> "PickCard",
                        "isDefaultPile" -> BooleanWire.encode(isDefaultPile) 
                    )
            

        override def decode(json: Value): Try[Event] = Try :
            json("type").str match
                case "Discard" =>
                    Discard(CardWire.decode(json("card")).get)
                case "PlayCard" => 
                    PlayCard(CardWire.decode(json("card")).get)
                case "PickCard" =>
                    val isDefaultPile: Boolean = json("isDefaultPile").bool
                    PickCard(isDefaultPile)
            
      

    override object viewFormat extends WireFormat[View]:
        val boardWire = MapWire(StringWire, VectorWire(CardWire))
        val handWire = SeqWire(CardWire)
        val winnersWire = SeqWire(StringWire)
        override def encode(v: View): Value =
            v.phaseView match
                case GameView(board: Board, hand: Hand, lastDiscard: Card, turnOf: UserId, drawPileSize: Int) =>
                    Obj(
                        "phaseView"    -> "GameView",
                        "board"        -> boardWire.encode(board),
                        "hand"         -> handWire.encode(hand),
                        "lastDiscard"  -> CardWire.encode(lastDiscard), 
                        "turnOf"       -> StringWire.encode(turnOf),
                        "drawPileSize" -> IntWire.encode(drawPileSize)
                    )
                case VictoryView(winners: List[UserId]) =>
                    Obj(
                        "phaseView" -> "VictoryView",
                        "winners"   -> winnersWire.encode(winners) 
                    )

        
        override def decode(json: Value): Try[View] = Try:
            json("phaseView").str match
                case "GameView" =>
                    val board: Board = boardWire.decode(json("board")).get
                    val hand: Hand = handWire.decode(json("hand")).get.toVector
                    val lastDiscard: Card = CardWire.decode(json("lastDiscard")).get
                    val turnOf: UserId = StringWire.decode(json("turnOf")).get
                    val drawPileSize: Int = IntWire.decode(json("drawPileSize")).get
                    View(GameView(board,hand,lastDiscard,turnOf,drawPileSize))  

                case "VictoryView" =>
                    val winners: List[UserId] = winnersWire.decode(json("winners")).get.toList
                    View(VictoryView(winners))

                
            
            

object CardWire extends WireFormat[Card]:
    override def encode(card: Card): Value = 
        val attributes = card match
            case Money(amount) =>
                Obj(
                    "amount" -> IntWire.encode(amount)
                )
            case Profession(studyRequired, salary) =>
                Obj(
                    "studyRequired" -> IntWire.encode(studyRequired),
                    "salary" -> IntWire.encode(salary)
                )
            // case Study =>
            // case Flirt => 
            // case Child =>
            // case Pet =>
            // case Malus =>
            // case Special =>
            case _ => Obj()
        
        Obj("card" -> card.productPrefix, "attributes" -> attributes)

    override def decode(json: Value): Try[Card] = Try:
        json("card").str match
            case "Money" =>
                val amount: Int = IntWire.decode(json("attributes")("amount")).get
                Money(amount)

            case "Profession" =>
                val studyRequired = IntWire.decode(json("attributes")("studyRequired")).get
                val salary = IntWire.decode(json("attributes")("salary")).get
                Profession(studyRequired, salary)

            case "Study" => Study
            case "Flirt" => Flirt
            case "Child" => Child
            case "Pet" => Pet
            case "Malus" => Malus
            case "Special" => Special