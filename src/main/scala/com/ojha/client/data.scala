package com.ojha.client

/**
 * Created by alexandra on 04/01/16.
 */

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import spray.json._
import DefaultJsonProtocol._


/* The basic response format with ok and optional error message */

trait StarFighterResponse {
  val ok: Boolean
  val data: Either[ErrorMessage, Data]
}

case class ErrorMessage(msg: String)

trait Data

/* -----------------------------------------------------------------
    Serialization for joda datetime
   ----------------------------------------------------------------- */

object FractionalSecondsDateTimeProtocol extends DefaultJsonProtocol {
  implicit object dateTimeFormat extends RootJsonFormat[DateTime] {
    private val pattern = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
    override def write(obj: DateTime): JsValue =
      JsString(pattern.print(obj))

    override def read(json: JsValue): DateTime = json match {
      case JsString(s) if s(19) != '+' =>
        pattern.parseDateTime(s.substring(0,23)) // todo hack til i can handle fractional seconds piglet
      case JsString(s)=>
        DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss").parseDateTime(s.substring(0,19)) // todo hack til i can handle fractional seconds piglet
      case _ => deserializationError("Cannot deserialize DateTime")
    }
  }
}

//object ISO8601DateTimeProtocol extends DefaultJsonProtocol {
//  implicit object dateTimeFormat extends RootJsonFormat[DateTime] {
//    private val pattern = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
//    override def write(obj: DateTime): JsValue =
//      JsString(pattern.print(obj))
//
//    override def read(json: JsValue): DateTime = json match {
//      case JsString(s) => pattern.parseDateTime(s.takeWhile(_ != '+')) // todo hack becuase i dont know what it is piglet
//      case _ => deserializationError("Cannot deserialize DateTime")
//    }
//  }
//}

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
      import SymbolProtocol._
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


/* -----------------------------------------------------------------
    Get orderbook for a venue: data model and serialisation protocols
   ----------------------------------------------------------------- */

trait OutstandingOrder {
  val price: BigInt
  val qty: Int
  val isBuy: Boolean
}

case class Bid(override val price: BigInt, override val qty: Int) extends OutstandingOrder {
  override val isBuy = true
}

object BidProtocol extends DefaultJsonProtocol {
  implicit object bidFormat extends RootJsonFormat[Bid] {
    override def write(obj: Bid): JsValue = {
      JsObject (
        "price" -> JsNumber(obj.price),
        "qty" -> JsNumber(obj.qty),
        "isBuy" -> JsBoolean(obj.isBuy)
      )
    }

    override def read(json: JsValue): Bid = {
      val fields = json.asJsObject.fields
      (fields.get("price"), fields.get("qty")) match {
        case (Some(JsNumber(p)), Some(JsNumber(q))) => Bid(p.toBigInt(), q.toInt)
        case msg@_ =>
          deserializationError("Unable to deserialize Bid")
      }
    }
  }
}

case class Ask(override val price: BigInt, override val qty: Int) extends OutstandingOrder {
  override val isBuy = false
}

object AskProtocol extends DefaultJsonProtocol {

  implicit object askFormat extends RootJsonFormat[Ask] {
    override def write(obj: Ask): JsValue = {
      val jsObj = JsObject (
        "price" -> JsNumber(obj.price),
        "qty" -> JsNumber(obj.qty),
        "isBuy" -> JsBoolean(obj.isBuy)
      )
      jsObj
    }

    override def read(json: JsValue): Ask = {
      val fields = json.asJsObject.fields
      (fields.get("price"), fields.get("qty")) match {
        case (Some(JsNumber(p)), Some(JsNumber(q))) => Ask(p.toBigInt(), q.toInt)
        case _ => deserializationError("Unable to deserialize Ask")
      }
    }
  }
}

case class OrderBookData(venue: String, symbol: String, bids: Option[Seq[Bid]], asks: Option[Seq[Ask]], ts: DateTime) extends Data

object OrderBookDataProtocol extends DefaultJsonProtocol {
  import BidProtocol._
  import AskProtocol._
  import FractionalSecondsDateTimeProtocol._

  implicit val orderBookDataFormat: RootJsonFormat[OrderBookData] = jsonFormat5(OrderBookData)
}

case class OrderBookResponse(override val ok: Boolean,
                             override val data: Either[ErrorMessage, OrderBookData]) extends StarFighterResponse

object OrderBookResponseProtocol extends DefaultJsonProtocol {
  implicit object orderBookResponseFormat extends RootJsonFormat[OrderBookResponse]  {
    import OrderBookDataProtocol._
    override def read(json: JsValue): OrderBookResponse
    = Rawr.read[OrderBookResponse, OrderBookData](json, (b: Boolean, e: Either[ErrorMessage, OrderBookData]) => new OrderBookResponse(b,e))
    override def write(obj: OrderBookResponse): JsValue
    = Rawr.write[OrderBookResponse, OrderBookData](obj)
  }
}

/* -----------------------------------------------------------------
    OrderDirection
   ----------------------------------------------------------------- */

trait Direction

object Directions {
  case object Buy extends Direction {
    override def toString = "buy"
  }
  case object Sell extends Direction {
    override def toString = "sell"
  }
}

object DirectionProtocol extends DefaultJsonProtocol {
  implicit object directionFormat extends RootJsonFormat[Direction] {
    override def write(obj: Direction): JsValue = JsString(obj.toString)

    override def read(json: JsValue): Direction = json match {
      case JsString("sell") => Directions.Sell
      case JsString("buy") => Directions.Buy
      case _ => deserializationError("Cannot deserialize Direction")
    }
  }
}

/* -----------------------------------------------------------------
    OrderType
   ----------------------------------------------------------------- */

trait OrderType
object OrderTypes {
  case object Limit extends OrderType {
    override def toString = "limit"
  }
  case object Market extends OrderType {
    override def toString = "market"
  }
  case object IoC extends OrderType {
    override def toString = "immediate-or-cancel"
  }
}

object OrderTypeProtocol extends DefaultJsonProtocol {
  implicit object orderTypeFormat extends RootJsonFormat[OrderType] {
    override def write(obj: OrderType): JsValue = JsString(obj.toString)

    override def read(json: JsValue): OrderType = json match {
      case JsString("limit") => OrderTypes.Limit
      case JsString("market") => OrderTypes.Market
      case JsString("immediate-or-cancel") => OrderTypes.IoC
      case _ => deserializationError("Cannot deserialize OrderType")
    }
  }
}

/* -----------------------------------------------------------------
    Placing a new order: data model and serialisation protocols
   ----------------------------------------------------------------- */

case class Fill(price: Int, qty: Int, ts: DateTime)

object FillProtocol extends DefaultJsonProtocol {
  import FractionalSecondsDateTimeProtocol._
  implicit val fillFormat = jsonFormat3(Fill)
}

case class NewOrder(account: String,
                    venue: String,
                    stock: String,
                    price: Int,
                    qty: Int,
                    direction: Direction,
                    orderType: OrderType)

object NewOrderProtocol extends DefaultJsonProtocol {

  import OrderTypeProtocol._
  import DirectionProtocol._

  implicit val newOrderFormat = jsonFormat7(NewOrder)

}

case class NewOrderData(symbol: String,
                    venue: String,
                    direction: Direction,
                    originalQty: Int,
                    qty: Int,
                    orderType: OrderType,
                    id: Int,
                    account: String,
                    ts: DateTime,
                    fills: Seq[Fill],
                    totalFilled: Int,
                    open: Boolean) extends Data

object NewOrderDataProtocol extends DefaultJsonProtocol {
  import OrderTypeProtocol._
  import DirectionProtocol._
  import FractionalSecondsDateTimeProtocol._
  import FillProtocol._
  implicit val newOrderDataFormat = jsonFormat12(NewOrderData)
}

case class NewOrderResponse(override val ok: Boolean,
                            override val data: Either[ErrorMessage, NewOrderData]) extends StarFighterResponse


object NewOrderResponseProtocol extends DefaultJsonProtocol {
  implicit object newOrderResponseProtocol extends RootJsonFormat[NewOrderResponse]  {
    import NewOrderDataProtocol._
    override def read(json: JsValue): NewOrderResponse
      = Rawr.read[NewOrderResponse, NewOrderData](json, (b: Boolean, e: Either[ErrorMessage, NewOrderData]) => new NewOrderResponse(b,e))
    override def write(obj: NewOrderResponse): JsValue
      = Rawr.write[NewOrderResponse, NewOrderData](obj)
  }
}

/* -----------------------------------------------------------------
    Getting a quote for a stock: data model and serialisation protocols
   ----------------------------------------------------------------- */

case class StockQuoteData(symbol: String,
                          venue: String,
                          bid: Option[Int],
                          ask: Option[Int],
                          bidSize: Option[Int],
                          askSize: Option[Int],
                          bidDepth: Option[Int],
                          askDepth: Option[Int],
                          last: Option[Int],
                          lastSize: Option[Int],
                          lastTrade: Option[DateTime],
                          quoteTime: DateTime) extends Data

case class StockQuoteResponse(override val ok: Boolean,
                              override val data: Either[ErrorMessage, StockQuoteData]) extends StarFighterResponse

object StockQuoteDataProtocol extends DefaultJsonProtocol {
  import FractionalSecondsDateTimeProtocol._
  implicit val stockQuoteDataFormat = jsonFormat12(StockQuoteData)
}

object StockQuoteResponseProtocol extends DefaultJsonProtocol {
    implicit object stockQuoteResponseProtocol extends RootJsonFormat[StockQuoteResponse]  {
      import StockQuoteDataProtocol._
      override def read(json: JsValue): StockQuoteResponse
        = Rawr.read[StockQuoteResponse, StockQuoteData](json, (b: Boolean, e: Either[ErrorMessage, StockQuoteData]) => new StockQuoteResponse(b,e))
      override def write(obj: StockQuoteResponse): JsValue
        = Rawr.write[StockQuoteResponse, StockQuoteData](obj)
    }
}

/* -----------------------------------------------------------------
    Getting status for an order: data model and serialisation protocols
   ----------------------------------------------------------------- */

case class OrderStatusData(symbol: String,
                           venue: String,
                           direction: Direction,
                           originalQty: Int,
                           qty: Int,
                           price: Int,
                           orderType: OrderType,
                           id: Int,
                           account: String,
                           ts: DateTime,
                           fills: Seq[Fill],
                           totalFilled: Int,
                           open: Boolean) extends Data

case class OrderStatusResponse(override val ok: Boolean,
                              override val data: Either[ErrorMessage, OrderStatusData]) extends StarFighterResponse

object OrderStatusDataProtocol extends DefaultJsonProtocol {
  import FractionalSecondsDateTimeProtocol._
  import DirectionProtocol._
  import OrderTypeProtocol._
  implicit val fillFormat = jsonFormat3(Fill)
  implicit val orderStatusFormat = jsonFormat13(OrderStatusData)
}

object OrderStatusResponseProtocol extends DefaultJsonProtocol {
  implicit object orderStatusResponseProtocol extends RootJsonFormat[OrderStatusResponse]  {
    import OrderStatusDataProtocol._
    override def read(json: JsValue): OrderStatusResponse
      = Rawr.read[OrderStatusResponse, OrderStatusData](json, (b: Boolean, e: Either[ErrorMessage, OrderStatusData]) => new OrderStatusResponse(b,e))
    override def write(obj: OrderStatusResponse): JsValue
      = Rawr.write[OrderStatusResponse, OrderStatusData](obj)
  }
}
 /* -----------------------------------------------------------------
      Getting statuses for all order: data model and serialisation protocols
     ----------------------------------------------------------------- */

  case class AllOrderStatusesData(venue: String, orders: Seq[OrderStatusData]) extends Data

  case class AllOrderStatusesResponse(override val ok: Boolean,
                                 override val data: Either[ErrorMessage, AllOrderStatusesData]) extends StarFighterResponse

  object AllOrderStatusesDataProtocol extends DefaultJsonProtocol {
    import OrderStatusDataProtocol._
    implicit val allOrderStatusFormat: RootJsonFormat[AllOrderStatusesData] = jsonFormat2(AllOrderStatusesData)
  }

  object AllOrderStatusResponseProtocol extends DefaultJsonProtocol {

    implicit object allorderStatusResponseFormat extends RootJsonFormat[AllOrderStatusesResponse]  {
      import AllOrderStatusesDataProtocol._
      override def read(json: JsValue): AllOrderStatusesResponse
        = Rawr.read[AllOrderStatusesResponse, AllOrderStatusesData](json, (b: Boolean, e: Either[ErrorMessage, AllOrderStatusesData]) => new AllOrderStatusesResponse(b,e))
      override def write(obj: AllOrderStatusesResponse): JsValue
        = Rawr.write[AllOrderStatusesResponse, AllOrderStatusesData](obj)
    }
  }


object Rawr {

  def write[T <: StarFighterResponse, U <: Data](obj: T)
                                                (implicit dataFormat: RootJsonFormat[U] ): JsValue = {
    JsObject(Map("ok" -> JsBoolean(obj.ok)) ++ (obj.data match {
      case Left(e) => Map("error" -> JsString(e.msg))
      case Right(d) => d.asInstanceOf[U].toJson.asJsObject.fields
    }))
  }

  def read[T <: StarFighterResponse, U <: Data](json: JsValue,
                                                f: (Boolean, Either[ErrorMessage, U]) => T)
                                               (implicit dataFormat: RootJsonFormat[U]): T = {
    val fields = json.asJsObject.fields
    (fields.get("ok"), fields.get("error")) match {
      case (Some(JsBoolean(b)), Some(JsString(e))) => f(b, Left(ErrorMessage(e)))
      case (Some(JsBoolean(b)), None) => {
        val dataFields = fields.filter(_._1 != "ok")
        val dataObj = JsObject(dataFields)
        val data = dataObj.convertTo[U]
        f(b, Right(data))
      }
      case _ => deserializationError(s"Deserialization error")
    }
  }

}

