package com.ojha.client

import com.typesafe.scalalogging.LazyLogging
import dispatch._

import spray.json._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by alexandra on 04/01/16.
 */

object StarFighterClient extends Configurable {
  def apply(): StarFighterClient = {
    val baseurl = config.getString("starfighter.baseurl")
    val apikey = config.getString("starfighter.apikey")
    new StarFighterClient(baseurl, apikey)
  }
}

class StarFighterClient(baseurl: String, apikey: String) extends LazyLogging {

  logger.info(s"Started StarFighterClient with baseurl $baseurl")

  def heartBeat(): Future[Heartbeat] = {
    import com.ojha.client.HeartbeatProtocol._
    val request = url(s"$baseurl/heartbeat")
    logger.info(s"Heartbeating $baseurl/heartbeat")
    execute[Heartbeat](request)
  }

  def heartBeatVenue(venue: String): Future[VenueHeartbeatResponse] = {
    import com.ojha.client.VenueHeartbeatResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/heartbeat"
    val request = url(urlPath)
    logger.info(s"Heartbeating venue $urlPath")
    execute[VenueHeartbeatResponse](request)
  }

  def stocksOnAVenue(venue: String): Future[StocksInfoResponse] = {
    import com.ojha.client.StocksInfoResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Checking stocks on venue $urlPath")
    execute[StocksInfoResponse](request)
  }

  def orderBookForStockOnVenue(venue: String, stock: String): Future[OrderBookResponse] = {
    import com.ojha.client.OrderBookResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Checking orderbook for stock on venue $urlPath")
    execute[OrderBookResponse](request)
  }

  def placeOrderForStock(venue: String, stock: String, order: NewOrder): Future[NewOrderResponse] = {
    import com.ojha.client.NewOrderProtocol._
    import NewOrderResponseProtocol._
    val orderJson = order.toJson.compactPrint
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/orders"
    val request = url(urlPath).setBody(orderJson).setHeader("X-Starfighter-Authorization", apikey).POST
    logger.info(s"Posting an order for stock at url: $urlPath, order:$orderJson ")
    execute[NewOrderResponse](request)
  }

  def getQuoteForStock(venue: String, stock: String): Future[StockQuoteResponse] = {
    import StockQuoteResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/quote"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Getting quote for stock $urlPath")
    execute[StockQuoteResponse](request)
  }

  def getStatusForOrder(venue: String, stock: String, id: Int): Future[OrderStatusResponse]= {
    import OrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/orders/$id"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Status for order $urlPath")
    execute[OrderStatusResponse](request)
  }

  def cancelOrder(venue: String, stock: String, id: Int): Future[OrderStatusResponse] = {
    import OrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/orders/$id"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey).DELETE
    logger.info(s"Cancel order $urlPath")
    execute[OrderStatusResponse](request)
  }

  def getAllOrderStatuses(venue: String, account: String): Future[AllOrderStatusesResponse] = {
    import AllOrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/accounts/$account/orders"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Getting all orders on venue for an account $urlPath")
    execute[AllOrderStatusesResponse](request)

  }


  def getAllOrderStatusesForStock(venue: String, stock: String, account: String): Future[AllOrderStatusesResponse] = {
    import AllOrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/accounts/$account/stocks/$stock/orders"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Getting all orders on venue for a stock/account $urlPath")
    execute[AllOrderStatusesResponse](request)
  }


  private def execute[T](request: Req)(implicit format: JsonFormat[T]): Future[T] = {
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[T])
  }

  def shutdown(): Unit = {
    logger.info("Shutting down StarFighterClient")
    dispatch.Http.shutdown()
  }



}
