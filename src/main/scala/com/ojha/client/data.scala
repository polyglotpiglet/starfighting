package com.ojha.client

/**
 * Created by alexandra on 04/01/16.
 */

import com.ojha.client.SymbolProtocol._
import org.joda.time.DateTime
import spray.json._

case class Fill(price: Int, qty: Int, ts: DateTime)

/* The basic response format with ok and optional error message */
trait StarFighterResponse {
  val ok: Boolean
  val data: Either[ErrorMessage, Data]
}

case class ErrorMessage(msg: String)

trait Data

case class Heartbeat(override val ok: Boolean, override val data: Either[ErrorMessage, Data]) extends StarFighterResponse

/* Json formatting for the Heartbeat */
object HeartbeatProtocol extends DefaultJsonProtocol {
  implicit val format = jsonFormat2(Heartbeat)
}


/* Response object for heartbeating the venue api */

case class VenueHeartbeatData(venue: String) extends Data
case class VenueHeartbeatResponse(override val ok: Boolean, override val data: Either[ErrorMessage, VenueHeartbeatData]) extends StarFighterResponse

object VenueHeartbeatDataProtocol {
  implicit val symbolFormat: RootJsonFormat[VenueHeartbeatData] = jsonFormat1(VenueHeartbeatData)
}

/* Json formatter for the VenueHeartbeatResponse */
object VenueHeartbeatResponseProtocol extends DefaultJsonProtocol {
  implicit object format extends RootJsonFormat[VenueHeartbeatResponse] {
    def write(r: VenueHeartbeatResponse) = {
      JsObject {
        "ok" -> JsBoolean(r.ok)
        r.data match {
          case Left(e) => "error" -> JsString(e.msg)
          case Right(d) => "venue" -> JsString(d.venue)
        }
      }
    }

    def read(value: JsValue): VenueHeartbeatResponse = {
      val fields = value.asJsObject.fields
      val ok: JsValue = fields.getOrElse("ok", deserializationError("Need mandatory field ok in VenueReponse"))
      val error = fields.get("error")
      val venue = fields.get("venue")
      null

    }
  }
}

/* Response object for info about stocks on a particular venue */
case class StocksInfoResponse(override val ok: Boolean, symbols: Option[Seq[Symbol]] = None) extends StarFighterResponse

/* Basic info about a particular symbol */
case class Symbol(name: String, symbol: String)

object StocksInfoProtocol {
  implicit object format extends RootJsonFormat[StocksInfoResponse] {

    import SymbolProtocol._
    override def write(obj: StocksInfoResponse): JsValue = JsObject {
      "ok" -> JsBoolean(obj.ok)
      "symbols" -> JsArray(obj.symbols.map(_.map(_.toJson).toVector).getOrElse(Vector()))
    }

    override def read(json: JsValue): StocksInfoResponse = {
      val fields = json.asJsObject.fields
      val ok = fields.getOrElse("ok", deserializationError("StockInfoResponse missing mandatory ok")) match {
        case JsBoolean(s) => s
        case _ => deserializationError("nable to deserialize ok in StockInfoResponse")
      }
      val error: Option[String] = fields.get("error") match {
        case Some(JsString(s)) => Some(s)
        case None => None
        case _ => deserializationError("Unable to deserialize error in StockInfoResponse")
      }

      val symbols: Option[Vector[Symbol]] = fields.get("symbols") match {
        case Some(JsArray(v)) => Some(v.map {
          case o: JsObject => o.convertTo[Symbol]
          case _ => deserializationError("Unable to deserialize seq of symbols in StockInfo")
        })
        case _ => None
      }
      StocksInfoResponse(ok, symbols)

    }
  }
}

object SymbolProtocol extends DefaultJsonProtocol  {
  implicit val symbolFormat: RootJsonFormat[Symbol] = jsonFormat2(Symbol)
}

//case class OrderBook(override val ok: Boolean,
//                     override val error: Option[String] = None
//                      venue: Option[String]) extends StarFighterResponse




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