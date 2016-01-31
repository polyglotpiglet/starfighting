package com.ojha.tradingstrategies

import com.ojha.client._
import com.typesafe.scalalogging.LazyLogging
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.{Success, Failure}

/**
 * Created by alexandra on 30/01/16.
 */
// this is such bad code and algo, don't judge gah but it worked
class ChockABlock extends TradingStrategy with LazyLogging {

  val account = "KAS77935519"
  val stock = "SEEC"

  val sharesToBuy = 100000
  val venue = "OFABEX"

  lazy val client = StarFighterClient()

  override def execute(): Unit = {

    var originalPrice: Integer = null
    var bought = 0

    def tryBuy(): Unit = {
      println(s"Bought = $bought")

      val stockQuote = client.getQuoteForStock(venue, stock)
      stockQuote.onComplete {
        case Success(quote) => quote.data match {
          case Right(data) => handleStockQuoteResponse(data)
          case Left(e) => logger.error(e.msg)
        }
        case Failure(e) => logger.error(e.getMessage)
      }

      def handleStockQuoteResponse(data: StockQuoteData): Unit = {
        if (originalPrice == null) { data.bid.foreach(b => originalPrice = b) ; tryBuy() }
        // if price has gone up 10%
        else if (data.bid.isDefined && data.bid.get > originalPrice + originalPrice / 100 * 1) {
          Thread.sleep(3000)
          tryBuy()
        }
        else {
          (data.ask, data.askSize) match {
            case (Some(a), Some(prevSize)) => {
              val size = math.min(1000, sharesToBuy - bought)
              val order = client.placeOrderForStock(venue, stock, new NewOrder(account, venue, stock, a, size, Directions.Buy, OrderTypes.IoC))

              order.onComplete {
                case Success(o) => {
                  o.data match {
                    case Right(d) => {
                      bought += d.totalFilled
                      logger.info(s"New order info: id = ${d.id}, open = ${d.open}, filled = ${d.totalFilled}")
                      d.fills.foreach { f =>
                        println(s"price = ${f.price} qty = ${f.qty}")
                      }
                      if (bought < sharesToBuy) Thread.sleep(3000); tryBuy()
                    }
                    case Left(e) => logger.error(e.msg)
                  }
                }
                case Failure(e) => logger.error(e.getMessage)
              }
            }
            case _ => tryBuy()
          }

            }
          }
        }

    tryBuy()

  }
}
