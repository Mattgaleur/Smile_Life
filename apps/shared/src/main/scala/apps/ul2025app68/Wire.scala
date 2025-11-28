package apps
package ul2025app68

import cs214.webapp.*
import cs214.webapp.DecodingException

import scala.util.{Failure, Success, Try}
import ujson.Value
import ujson.Obj

object Wire extends AppWire[Event, View]:
    import Event.*
    import View.*
    import ujson.*

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
        override def encode(v: View): Value =
        v match
            case View(board, hand) => 
                Obj(
                    "board" -> boardWire.encode(board),
                    "hand" -> handWire.encode(hand)
                )

        override def decode(json: Value): Try[View] = Try:
            val board: Board = boardWire.decode(json("board")).get
            val hand: Hand = handWire.decode(json("hand")).get.toVector
            View(board,hand)  
            

object CardWire extends WireFormat[Card]:
    override def encode(t: Card): Value = 
        IntWire.encode(t.ordinal)

    override def decode(json: Value): Try[Card] = Try:
        Card.fromOrdinal(
            IntWire.decode(json).get
        )