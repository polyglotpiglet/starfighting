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
    new StarFighterClient(baseurl)
  }
}

class StarFighterClient(baseurl: String) extends LazyLogging {

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
    val request = url(urlPath)
    logger.info(s"Checking stocks on venue $urlPath")
    val result = Http(request)
    result.map(_.getResponseBody.parseJson.convertTo[StocksInfoResponse])
  }

  def execute(order: Order) = {
    val request = Request(order)
  }

  def shutdown(): Unit = {
    logger.info("Shutting down StarFighterClient")
    dispatch.Http.shutdown()
  }



}
