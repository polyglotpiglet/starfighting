package com.ojha.client

/**
 * Created by alexandra on 04/01/16.
 */

import org.joda.time.DateTime
import spray.json._
import DefaultJsonProtocol._

case class Fill(price: Int, qty: Int, ts: DateTime)

/* The basic response format with ok and optional error message */

trait StarFighterResponse {
  val ok: Boolean
  val data: Either[ErrorMessage, Data]
}

case class ErrorMessage(msg: String)

trait Data

/* -----------------------------------------------------------------
    API Heartbeating data model and serialisation protocols
   ----------------------------------------------------------------- */

case class Heartbeat(override val ok: Boolean, override val data: Either[ErrorMessage, Data]) extends StarFighterResponse

/* Json formatting for the Heartbeat */
object HeartbeatProtocol extends DefaultJsonProtocol {
  implicit object heartbeatFormat extends RootJsonFormat[Heartbeat] {
    override def read(json: JsValue): Heartbeat = {
      val fields = json.asJsObject.fields
      val ok = fields.getOrElse("ok", deserializationError("No ok in Heartbeat"))
      val error = fields.get("error")
      (ok, error) match {
        case (JsBoolean(b), Some(JsString(e))) => Heartbeat(b, Left(ErrorMessage(e)))
        case (JsBoolean(b), None) => Heartbeat(b, Left(ErrorMessage("")))
        case _ => deserializationError("Cannot deserialize heartbeat")
      }
    }

    override def write(obj: Heartbeat): JsValue = JsObject {
        "ok" -> JsBoolean(obj.ok)
        obj.data match {
          case Left(e) => "error" -> JsString(e.msg)
          case _ => serializationError("Unable to serialize Heartbeat")
        }
      }
  }
}

/* -----------------------------------------------------------------
    Venue Heartbeating data model and serialisation protocols
   ----------------------------------------------------------------- */

case class VenueHeartbeatData(venue: String) extends Data
case class VenueHeartbeatResponse(override val ok: Boolean,
                                  override val data: Either[ErrorMessage, VenueHeartbeatData]) extends StarFighterResponse

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
      (ok, error, venue) match {
        case (JsBoolean(b), Some(JsString(e)), None) => VenueHeartbeatResponse(b, Left(ErrorMessage(e)))
        case (JsBoolean(b), None, Some(JsString(v))) => VenueHeartbeatResponse(b, Right(VenueHeartbeatData(v)))
        case _ => deserializationError("Could not read VenueHeartBeatResponse")

      }
    }
  }
}


/* -----------------------------------------------------------------
    Stocks info data model and serialisation protocols
   ----------------------------------------------------------------- */

case class Symbol(name: String, symbol: String)

case class StocksInfoData(symbols: Seq[Symbol]) extends Data

case class StocksInfoResponse(override val ok: Boolean, override val data: Either[ErrorMessage, StocksInfoData]) extends StarFighterResponse

object SymbolProtocol extends DefaultJsonProtocol  {
  implicit val symbolFormat: RootJsonFormat[Symbol] = jsonFormat2(Symbol)
}

object StocksInfoDataProtocol extends DefaultJsonProtocol {
  import SymbolProtocol._
  implicit val stocksInfoDataFromat: RootJsonFormat[StocksInfoData] = jsonFormat1(StocksInfoData)
}



object StocksInfoResponseProtocol extends DefaultJsonProtocol {
  import StocksInfoDataProtocol._
  implicit object stocksInfoResponseFormat extends RootJsonFormat[StocksInfoResponse] {
    override def write(obj: StocksInfoResponse): JsValue = {
      JsObject {
        "ok" -> JsBoolean(obj.ok)
        obj.data match {
          case Left(e) => "error" -> JsString(e.msg)
          case Right(d) => "symbols" -> d.toJson
        }
      }
    }

    override def read(json: JsValue): StocksInfoResponse = {
      val fields = json.asJsObject.fields
      val ok = fields.getOrElse("ok", deserializationError("No ok in StocksInfoResponse"))
      val error = fields.get("error")
      val venue = fields.get("symbols")
      (ok, error, venue) match {
        case (JsBoolean(b), Some(JsString(e)), None) => StocksInfoResponse(b, Left(ErrorMessage(e)))
        case (JsBoolean(b), None, Some(JsArray(v))) => StocksInfoResponse(b, Right(StocksInfoData(v.map(_.convertTo[Symbol]))))
        case _ => deserializationError("Unable to generate StocksInfoResponse missing data")
      }
    }
  }
}

//
///* Response object for info about stocks on a particular venue */
//case class StocksInfoResponse(override val ok: Boolean, symbols: Option[Seq[Symbol]] = None) extends StarFighterResponse
//
///* Basic info about a particular symbol */
//
//object StocksInfoProtocol {
//  implicit object format extends RootJsonFormat[StocksInfoResponse] {
//
//    import SymbolProtocol._
//    override def write(obj: StocksInfoResponse): JsValue = JsObject {
//      "ok" -> JsBoolean(obj.ok)
//      "symbols" -> JsArray(obj.symbols.map(_.map(_.toJson).toVector).getOrElse(Vector()))
//    }
//
//    override def read(json: JsValue): StocksInfoResponse = {
//      val fields = json.asJsObject.fields
//      val ok = fields.getOrElse("ok", deserializationError("StockInfoResponse missing mandatory ok")) match {
//        case JsBoolean(s) => s
//        case _ => deserializationError("nable to deserialize ok in StockInfoResponse")
//      }
//      val error: Option[String] = fields.get("error") match {
//        case Some(JsString(s)) => Some(s)
//        case None => None
//        case _ => deserializationError("Unable to deserialize error in StockInfoResponse")
//      }
//
//      val symbols: Option[Vector[Symbol]] = fields.get("symbols") match {
//        case Some(JsArray(v)) => Some(v.map {
//          case o: JsObject => o.convertTo[Symbol]
//          case _ => deserializationError("Unable to deserialize seq of symbols in StockInfo")
//        })
//        case _ => None
//      }
//      StocksInfoResponse(ok, symbols)
//
//    }
//  }
//}
//
//object SymbolProtocol extends DefaultJsonProtocol  {
//  implicit val symbolFormat: RootJsonFormat[Symbol] = jsonFormat2(Symbol)
//}

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