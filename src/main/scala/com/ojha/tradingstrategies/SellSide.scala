package com.ojha.tradingstrategies

import com.ojha.client.StarFighterClient
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by alexandra on 01/02/16.
 */
class SellSide extends TradingStrategy with LazyLogging {

  val targetProfit = 10000
  val shareLimit = 1000

  lazy val client = StarFighterClient()

  var profit = 0
  var basis = 0
  var numberOfShares = 0

  val venue = "OGWEX"
  val stock = "HAC"
  val account = "SOB39549030"

  override def execute(): Unit = {

    client.orderBookForStockOnVenue(venue, stock).onSuccess { case ob =>
      ob.data match {
        case Left(e) => logger.error(e.msg)
        case Right(r) => {

          (r.asks, r.bids) match {
            case (Some(a), Some(b)) if a.nonEmpty && b.nonEmpty => {
              val minAsk = a.head
              val maxBid = b.head

            }
            case _ =>
          }





        }
      }
    }
  }

  def withinLimits(): Boolean = {
    math.abs(numberOfShares) < shareLimit
  }
}
