package com.ojha.client

/**
 * Created by alexandra on 04/01/16.
 */

import org.joda.time.DateTime
import spray.json.DefaultJsonProtocol

case class Fill(price: Int, qty: Int, ts: DateTime)

case class Response(ok: Boolean, error: String)

object ResponseProtocol extends DefaultJsonProtocol {
  implicit val format = jsonFormat2(Response)
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