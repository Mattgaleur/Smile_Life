package apps
package ul2025app68

import cs214.webapp.*
import cs214.webapp.DecodingException

import scala.util.{Failure, Success, Try}

object Wire extends AppWire[Event, View]:
  import Event.*
  //import View.*
  import ujson.*

  override object eventFormat extends WireFormat[Event]:
    override def encode(event: Event): Value =
      ???      

    override def decode(json: Value): Try[Event] = Try :
      ???

//   override object viewFormat extends WireFormat[View]:
//     override def encode(v: View): Value =
//       ???

//     override def decode(json: Value): Try[View] = Try:
//       ???