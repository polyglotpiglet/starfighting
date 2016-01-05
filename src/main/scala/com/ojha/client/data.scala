package com.ojha.client

/**
 * Created by alexandra on 04/01/16.
 */

import org.joda.time.DateTime
import spray.json._

case class Fill(price: Int, qty: Int, ts: DateTime)


trait StarFighterResponse {
  val ok: Boolean
  val error: Option[String]
}

case class VanillaResponse(override val ok: Boolean, override val error: Option[String] = None) extends StarFighterResponse {
}

object VanillaResponseProtocol extends DefaultJsonProtocol {
  implicit val format = jsonFormat2(VanillaResponse)
}

case class VenueResponse(override val ok: Boolean, override val error: Option[String] = None, venue: Option[String] = None) extends StarFighterResponse

object VenueResponseProtocol extends DefaultJsonProtocol {
  implicit object format extends RootJsonFormat[VenueResponse] {
    def write(r: VenueResponse) = JsObject {
      "ok" -> JsBoolean(r.ok)
      "error" -> JsString(r.error.getOrElse(""))
      "venue" -> JsString(r.venue.getOrElse(""))
    }

    def read(value: JsValue) = {
      val fields = value.asJsObject.fields
      (fields.getOrElse("ok", deserializationError("Need mandatory field ok in VenueReponse")), fields.get("error"), fields.get("venue")) match {
        case (JsBoolean(b), Some(JsString(e)), Some(JsString(v))) => VenueResponse(b, Some(e), Some(v))
        case (JsBoolean(b), None, Some(JsString(v))) => VenueResponse(b, None, Some(v))
        case (JsBoolean(b), Some(JsString(e)), None) => VenueResponse(b, Some(e), None)
        case _ => deserializationError("Cannot deserialize dodgy VenueResponse")
      }
    }
  }
}



//case class Response(ok: Boolean,
//                    symbol: String,
//                    venue: String,
//                    direction: Direction,
//                    originalQty: Int,
//                    qty: Int,
//                    orderType: OrderType,
//                    id: Int,
//                    account: String,
//                    ts: DateTime,
//                    fills: Seq[Fill],
//                    totalFilled: Int,
//                    open: Boolean)

trait Direction

object Directions {
  case object Buy extends Direction {
    override def toString = "buy"
  }
  case object Sell extends Direction {
    override def toString = "sell"
  }
}

trait OrderType
object OrderTypes {
  case object Limit extends OrderType {
    override def toString = "limit"
  }
  case object Market extends OrderType {
    override def toString = "market"
  }
}


case class Order(account: String,
                 venue: String,
                 symbol: String,
                 price: Int,
                 qty: Int,
                 direction: Direction,
                 orderType: OrderType)

case class Request(order: Order)