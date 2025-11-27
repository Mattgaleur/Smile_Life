package apps
package ul2025app68

import cs214.webapp.*
import cs214.webapp.DecodingException

import scala.util.{Failure, Success, Try}

object Wire extends AppWire[Event, View]:
  import Event.*
  import View.*
  import ujson.*

  override object eventFormat extends WireFormat[Event]:
    override def encode(event: Event): Value =
        event match
            case Discard(card) => 
                Obj("type" -> "Discard", "card.ordinal" -> card.ordinal)
            case PlayCard(card) =>
                Obj("type" -> "PlayCard", "card.ordinal" -> card.ordinal)
            case PickCard(isDefaultPile) =>
                Obj(
                    "type" -> "PickCard",
                    "isDefaultPile" -> BooleanWire.encode(isDefaultPile) 
                )
        

    override def decode(json: Value): Try[Event] = Try :
        json("type").str match
            case "Discard" =>
                val card : Card = Card.fromOrdinal(json("card.ordinal").num.toInt)
                Discard(card)
            case "PlayCard" => 
                val card : Card = Card.fromOrdinal(json("card.ordinal").num.toInt)
                PlayCard(card)
            case "PickCard" =>
                val isDefaultPile: Boolean = json("isDefaultPile").bool
                PickCard(isDefaultPile)
        
      

  override object viewFormat extends WireFormat[View]:
    val boardWire = MapWire(StringWire, VectorWire(IntWire))
    val handWire = SeqWire(IntWire)
    override def encode(v: View): Value =
      v match
        case View(board, hand) => 
            val boardWithOrdinal = board.map((id, playedHands) => (id, playedHands.map(_.ordinal)))
            val handWithOrdinal = hand.map(_.ordinal)
            Obj(
                "board" -> boardWire.encode(boardWithOrdinal),
                "hand" -> handWire.encode(handWithOrdinal)
            )

    override def decode(json: Value): Try[View] = Try:
        val boardWithOrdinal = boardWire.decode(json("board")).get
        val board: Board = boardWithOrdinal.map:
            (id, playedHands) => (id, playedHands.map(Card.fromOrdinal(_)))
        val handWithOrdinal = handWire.decode(json("hand")).get
        val hand: Hand = handWithOrdinal.map(Card.fromOrdinal(_)).toVector
        View(board,hand)  
            
        