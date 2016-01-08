package com.ojha.client

import org.joda.time.DateTime
import spray.json._
import spray.json.{JsValue, RootJsonFormat, DefaultJsonProtocol}

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by alexandra on 05/01/16.
 */
object Main extends App {
  import OrderBookDataProtocol._


  val dateTime = new DateTime().withYear(2015).withMonthOfYear(12).withDayOfMonth(4).withHourOfDay(9).withMinuteOfHour(2).withSecondOfMinute(16)
  val bids = List[Bid](Bid(5200, 1), Bid(815, 15), Bid(800, 12), Bid(800, 152))
  val asks = List[Ask](Ask(5205, 150), Ask(5205, 1), Ask(BigInt(1000000000000L), 99999))
  val orderData = OrderBookData("OGEX", "FAC", bids, asks, dateTime)

  import DateTimeProtocol._
  val djson = dateTime.toJson

  val bid = Bid(1,2)
  import BidProtocol._
  val bidJ = bid.toJson
  val s = bidJ.prettyPrint

  val json = orderData.toJson
  val str = json.prettyPrint
//  println(orderData.toJson.prettyPrint)

  import OrderBookResponseProtocol._

  val cat = OrderBookResponse(true, Right(orderData))
  println(cat.toJson.prettyPrint.parseJson.convertTo[OrderBookResponse])

}
