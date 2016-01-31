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
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[Heartbeat])
  }

  def heartBeatVenue(venue: String): Future[VenueHeartbeatResponse] = {
    import com.ojha.client.VenueHeartbeatResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/heartbeat"
    val request = url(urlPath)
    logger.info(s"Heartbeating venue $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[VenueHeartbeatResponse])
  }

  def stocksOnAVenue(venue: String): Future[StocksInfoResponse] = {
    import com.ojha.client.StocksInfoResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Checking stocks on venue $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[StocksInfoResponse])
  }

  def orderBookForStockOnVenue(venue: String, stock: String): Future[OrderBookResponse] = {
    import com.ojha.client.OrderBookResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Checking orderbook for stock on venue $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[OrderBookResponse])
  }

  def placeOrderForStock(venue: String, stock: String, order: NewOrder): Future[NewOrderResponse] = {
    import com.ojha.client.NewOrderProtocol._
    import NewOrderResponseProtocol._
    val orderJson = order.toJson.compactPrint
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/orders"
    val request = url(urlPath).setBody(orderJson).setHeader("X-Starfighter-Authorization", apikey).POST
    logger.info(s"Posting an order for stock at url: $urlPath, order:$orderJson ")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[NewOrderResponse])
  }

  def getQuoteForStock(venue: String, stock: String): Future[StockQuoteResponse] = {
    import StockQuoteResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/quote"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Getting quote for stock $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[StockQuoteResponse])
  }

  def getStatusForOrder(venue: String, stock: String, id: Int): Future[OrderStatusResponse]= {
    import OrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/orders/$id"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Status for order $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[OrderStatusResponse])

  }

  def cancelOrder(venue: String, stock: String, id: Int): Future[OrderStatusResponse] = {
    import OrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/stocks/$stock/orders/$id"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey).DELETE
    logger.info(s"Cancel order $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[OrderStatusResponse])
  }

  def getAllOrderStatuses(venue: String, account: String): Future[AllOrderStatusesResponse] = {
    import AllOrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/accounts/$account/orders"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Getting all orders on venue for an account $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[AllOrderStatusesResponse])
  }


  def getAllOrderStatusesForStock(venue: String, stock: String, account: String): Future[AllOrderStatusesResponse] = {
    import AllOrderStatusResponseProtocol._
    val urlPath = s"$baseurl/venues/$venue/accounts/$account/stocks/$stock/orders"
    val request = url(urlPath).setHeader("X-Starfighter-Authorization", apikey)
    logger.info(s"Getting all orders on venue for a stock/account $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[AllOrderStatusesResponse])
  }


  def shutdown(): Unit = {
    logger.info("Shutting down StarFighterClient")
    dispatch.Http.shutdown()
  }



}
