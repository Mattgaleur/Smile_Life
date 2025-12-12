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
import ul2025app68.Malus
import ul2025app68.Bonus.*

object Wire extends AppWire[Event, View]:

    override object eventFormat extends WireFormat[Event]:
        override def encode(event: Event): Value =
            event match
                case Discard(card) => 
                    Obj("type" -> "Discard", "card" -> CardWire.encode(card))
                case PlayCard(card, userId) =>
                    Obj("type" -> "PlayCard", "card" -> CardWire.encode(card), "userId" -> StringWire.encode(userId))
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
                    PlayCard(
                        CardWire.decode(json("card")).get,
                        StringWire.decode(json("userId")).get
                    )
                case "PickCard" =>
                    val isDefaultPile: Boolean = json("isDefaultPile").bool
                    PickCard(isDefaultPile)
            
      

    override object viewFormat extends WireFormat[View]:
        val BoardWire = MapWire(StringWire, VectorWire(CardWire))
        val HandWire = VectorWire(CardWire)
        val WinnersWire = SeqWire(StringWire)
        val LastDiscardWire = OptionWire(CardWire)
        override def encode(v: View): Value =
            v.phaseView match
                case GameView(board: Board, hand: Hand, lastDiscard: Option[Card], turnOf: UserId, drawPileSize: Int) =>
                    Obj(
                        "phaseView"    -> "GameView",
                        "board"        -> BoardWire.encode(board),
                        "hand"         -> HandWire.encode(hand),
                        "lastDiscard"  -> LastDiscardWire.encode(lastDiscard), 
                        "turnOf"       -> StringWire.encode(turnOf),
                        "drawPileSize" -> IntWire.encode(drawPileSize)
                    )
                case VictoryView(winners: List[UserId]) =>
                    Obj(
                        "phaseView" -> "VictoryView",
                        "winners"   -> WinnersWire.encode(winners) 
                    )

        
        override def decode(json: Value): Try[View] = Try:
            json("phaseView").str match
                case "GameView" =>
                    val board: Board = BoardWire.decode(json("board")).get
                    val hand: Hand = HandWire.decode(json("hand")).get
                    val lastDiscard: Option[Card] = LastDiscardWire.decode(json("lastDiscard")).get
                    val turnOf: UserId = StringWire.decode(json("turnOf")).get
                    val drawPileSize: Int = IntWire.decode(json("drawPileSize")).get
                    View(GameView(board,hand,lastDiscard,turnOf,drawPileSize))  

                case "VictoryView" =>
                    val winners: List[UserId] = WinnersWire.decode(json("winners")).get.toList
                    View(VictoryView(winners))

                
            
            

object CardWire extends WireFormat[Card]:
    val JobBonusWire = OptionWire(SeqWire(BonusWire))
    override def encode(card: Card): Value = 
        val attributes = card match
            case MalusCard(malus: Malus) =>
                Obj(
                    "malus" -> MalusWire.encode(malus)
                )
            case House(price: Int) =>
                Obj(
                    "price" -> IntWire.encode(price)
                )
            case Travel(price: Int) =>
                Obj(
                    "price" -> IntWire.encode(price)
                )
            case Special(bonus: Bonus, name: String) =>
                Obj(
                    "bonus" -> BonusWire.encode(bonus),
                    "name" -> StringWire.encode(name)
                )
            case Money(amount: Int, used: Boolean) =>
                Obj(
                    "amount" -> IntWire.encode(amount),
                    "used" -> BooleanWire.encode(used)
                )
            case Profession(studyRequired: Int, salary: Int, bonus: Option[Seq[Bonus]], name: String) =>
                Obj(
                    "studyRequired" -> IntWire.encode(studyRequired),
                    "salary" -> IntWire.encode(salary),
                    "bonus" -> JobBonusWire.encode(bonus),
                    "name" -> StringWire.encode(name)
                )
            case _ => Obj()
        
        Obj("card" -> card.productPrefix, "attributes" -> attributes)

    override def decode(json: Value): Try[Card] = Try:
        json("card").str match
            case "Money" =>
                val amount = IntWire.decode(json("attributes")("amount")).get
                val used = BooleanWire.decode(json("attributes")("used")).get
                Money(amount, used)

            case "Profession" =>
                val studyRequired = IntWire.decode(json("attributes")("studyRequired")).get
                val salary = IntWire.decode(json("attributes")("salary")).get
                val bonus = JobBonusWire.decode(json("attributes")("bonus")).get
                val name = StringWire.decode(json("attributes")("name")).get
                Profession(studyRequired, salary, bonus, name)

            case "MalusCard" => 
                val malus = MalusWire.decode(json("attributes")("malus")).get
                MalusCard(malus)

            case "Special" => 
                val bonus = BonusWire.decode(json("attributes")("bonus")).get
                val name = StringWire.decode(json("attributes")("name")).get
                Special(bonus, name)

            case "House" =>
                val price = IntWire.decode(json("attributes")("price")).get
                House(price)

            case "Travel" =>
                val price = IntWire.decode(json("attributes")("price")).get
                Travel(price)

            case "Marriage" => Marriage
            case "Study" => Study
            case "Flirt" => Flirt
            case "Child" => Child
            case "Pet" => Pet

object MalusWire extends WireFormat[Malus]:
    def encode(malus: Malus): Value = 
        Obj("ord" -> IntWire.encode(malus.ordinal))

    def decode(json: Value): Try[Malus] = Try:
        Malus.fromOrdinal(IntWire.decode(json("ord")).get)

object BonusWire extends WireFormat[Bonus]:
    def encode(bonus: Bonus): Value =
        bonus match
            case MalusProtection(malus: Malus) =>
                Obj("ord" -> IntWire.encode(-1), "malus" -> MalusWire.encode(malus))
            case _ =>
                Obj("ord" -> IntWire.encode(bonus.ordinal))

    def decode(json: Value): Try[Bonus] = Try:
        IntWire.decode(json("ord")).get match
            case -1 =>
                MalusProtection(MalusWire.decode(json("malus")).get)
            case ord: Int =>
                Bonus.fromOrdinal(ord)