package com.ojha.client


import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration._
import org.joda.time.DateTime
import org.scalatest.time.{Seconds, Span, Millis}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import org.scalatest.concurrent.ScalaFutures

import spray.json._

import scala.concurrent.Future

/**
 * Created by alexandra on 04/01/16.
 */
class StarFighterClientSpec extends FlatSpec with Matchers with BeforeAndAfterAll with ScalaFutures {

  val port = 8989
  val host = "localhost"
  val wireMockServer = new WireMockServer(wireMockConfig().port(port))

  implicit val defaultPatience =
    PatienceConfig(timeout = Span(2, Seconds), interval = Span(5, Millis))

  override def beforeAll(): Unit = {
    wireMockServer.start()
    WireMock.configureFor(host, port)
  }

  override def afterAll(): Unit = {
    wireMockServer.stop()
  }

  it should "return a response on heartbeat" in {
    // given
    val path = "/heartbeat"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"error\": \"\"\n}")))

    val sut = StarFighterClient()

    // when
    val response: dispatch.Future[Heartbeat] = sut.heartBeat()

    // then
    whenReady(response) { r =>
      r should equal(Heartbeat(ok = true, Left(ErrorMessage(""))))
    }
  }

  it should "return a response on venue heartbeat" in {

    // given
    val path = "/venues/TESTEX/heartbeat"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"venue\": \"TESTEX\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.heartBeatVenue("TESTEX")

    // then
    whenReady(response) { r =>
      r should equal(VenueHeartbeatResponse(ok = true, Right(VenueHeartbeatData("TESTEX"))))
    }
  }

  it should "return a response on venue heartbeat timeout" in {
    // given
    val path = "/venues/TESTEX/heartbeat"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(500)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"The venue appears to be non-responsive even though its server is up.\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.heartBeatVenue("TESTEX")

    // then
    whenReady(response) { r =>
      r should equal(VenueHeartbeatResponse(ok = false, Left(ErrorMessage("The venue appears to be non-responsive even though its server is up."))))
    }
  }

  it should "return a response on venue heartbeat when venue doesnt exist (404)" in {
    // given
    val path = "/venues/TESTEX/heartbeat"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(404)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"No venue exists with the symbol TESTEX.\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.heartBeatVenue("TESTEX")

    // then
    whenReady(response) { r =>
      r should equal(VenueHeartbeatResponse(ok = false, Left(ErrorMessage("No venue exists with the symbol TESTEX."))))
    }
  }


  it should "return a stocks info response when checking stocks on a particular venue" in {
    // given
    val path = "/venues/TESTEX/stocks"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"symbols\": [\n    {\n      \"name\": \"Foreign Owned Occulmancy\", \n     \"symbol\": \"FOO\"\n    },\n    {\n      \"name\": \"Best American Ricecookers\",\n      \"symbol\": \"BAR\"\n    },\n    {\n      \"name\": \"Badly Aliased Zebras\", \n      \"symbol\": \"BAZ\"\n    }\n  ] \n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.stocksOnAVenue("TESTEX")

    // then
    whenReady(response) { r =>

      r.ok should be(right = true)

      val symbols = r.data.right.get.symbols
      symbols.length should equal(3)

      val symbol1 = Symbol("Foreign Owned Occulmancy", "FOO")
      val symbol2 = Symbol("Best American Ricecookers", "BAR")
      val symbol3 = Symbol("Badly Aliased Zebras", "BAZ")
      symbols should equal(Seq(symbol1, symbol2, symbol3))

    }
  }

  it should "return a correctly populated stocks info response when 404 venue not found" in {
    // given
    val path = "/venues/TESTEX/stocks"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(404)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"No venue exists with the symbol OGEX\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.stocksOnAVenue("TESTEX")

    // then
    whenReady(response) { r =>
      r.ok should be(right = false)
      r.data.left.get.msg should equal("No venue exists with the symbol OGEX")
    }
  }

  it should "return an orderbook response checking stock S on venue V with empty asks and bids" in {
    // given
    val path = "/venues/TESTEX/stocks/FOOBAR"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"venue\": \"TESTEX\",\n  \"symbol\": \"FOOBAR\",\n  \"ts\": \"2016-01-08T11:04:16.636875884Z\",\n  \"bids\": null,\n  \"asks\": null\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.orderBookForStockOnVenue("TESTEX", "FOOBAR")

    // then
    whenReady(response) { r =>
      r.ok should be(right = true)

      val ts = new DateTime(2016, 1, 8, 11, 4, 16, 636)
      val bids = List[Bid]()
      val asks = List[Ask]()
      val orderData = OrderBookData("TESTEX", "FOOBAR", None, None, ts)

      r.data.right.get should equal(orderData)
    }
  }

  it should "return an orderbook response checking stock S on venue V" in {
    // given
    val path = "/venues/TESTEX/stocks/BAR"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"venue\": \"OGEX\",\n  \"symbol\": \"FAC\",\n  \"bids\": \n    [\n      {\"price\": 5200, \"qty\": 1, \"isBuy\": true},\n      {\"price\": 815, \"qty\": 15, \"isBuy\": true},\n      {\"price\": 800, \"qty\": 12, \"isBuy\": true},\n      {\"price\": 800, \"qty\": 152, \"isBuy\": true}       \n    ],\n  \"asks\":\n   [\n      {\"price\": 5205, \"qty\": 150, \"isBuy\": false},\n      {\"price\": 5205, \"qty\": 1, \"isBuy\": false},\n      {\"price\": 1000000000000, \"qty\": 99999, \"isBuy\": false}\n   ],\n  \"ts\": \"2015-12-04T09:02:16.680986205Z\" \n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.orderBookForStockOnVenue("TESTEX", "BAR")

    // then
    whenReady(response) { r =>
      r.ok should be(right = true)

      val dateTime = new DateTime().withYear(2015).withMonthOfYear(12).withDayOfMonth(4).withHourOfDay(9).withMinuteOfHour(2).withSecondOfMinute(16).withMillisOfSecond(680)
      val bids = List[Bid](Bid(5200, 1), Bid(815, 15), Bid(800, 12), Bid(800, 152))
      val asks = List[Ask](Ask(5205, 150), Ask(5205, 1), Ask(BigInt(1000000000000L), 99999))
      val orderData = OrderBookData("OGEX", "FAC", Some(bids), Some(asks), dateTime)

      r.data.right.get should equal(orderData)
    }
  }

  it should "return orderbook error response when 404 stock on found on venue" in {
    // given
    val path = "/venues/TESTEX/stocks/BAR"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(404)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"symbol BAR does not exist on venue FOOEX\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.orderBookForStockOnVenue("TESTEX", "BAR")


    // then
    whenReady(response) { r =>
      r.ok should be(right = false)
      r.data.left.get.msg should equal("symbol BAR does not exist on venue FOOEX")
    }
  }

  it should "post an order and parse happy response for a stock on a venue" in {
    // given
    val newOrder = NewOrder("OGB12345", "OGEX", "FAC", 5100, 100, Directions.Buy, OrderTypes.Limit)

    val path = "/venues/OGEX/stocks/FAC/orders"
    stubFor(post(urlEqualTo(path))
      .withHeader("X-Starfighter-Authorization", equalTo("dummy_auth")) // from config
      .withRequestBody(equalTo("{\"stock\":\"FAC\",\"price\":5100,\"direction\":\"buy\",\"qty\":100,\"account\":\"OGB12345\",\"orderType\":\"limit\",\"venue\":\"OGEX\"}"))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n\t\"ok\": true,\n\t\"symbol\": \"FAC\",\n\t\"venue\": \"OGEX\",\n\t\"direction\": \"buy\",\n\t\"originalQty\": 100,\n\t\"qty\": 20,\n\t\"price\": 5100,\n\t\"orderType\": \"limit\",\n\t\"id\": 12345,\n\t\"account\": \"OGB12345\",\n\t\"ts\": \"2015-07-05T22:16:18+00:00\",\n\t\"fills\": [{\n\t\t\"price\": 5050,\n\t\t\"qty\": 50,\n\t\t\"ts\": \"2015-07-05T22:16:18+00:00\"\n\t}],\n\t\"totalFilled\": 80,\n\t\"open\": true\n}")))


    val sut = StarFighterClient()

    // when
    val response = sut.placeOrderForStock("OGEX", "FAC", newOrder)

    // then

    whenReady(response) { r =>

      val ts = new DateTime(2015, 7, 5, 22, 16, 18)

      val expected = NewOrderResponse(ok = true, Right(
        NewOrderData( "FAC",
          "OGEX",
          Directions.Buy,
          100,
          20,
          OrderTypes.Limit,
          12345,
          "OGB12345",
          ts,
          List(Fill(5050, 50, ts)),
          80,
          open = true)))

      r should equal(expected)
    }
  }

  it should "post an order and parse sad response for stocks on venue" in {
    // given
    val newOrder = NewOrder("OGB12345", "OGEX", "FAC", 5100, 100, Directions.Buy, OrderTypes.Limit)

    val path = "/venues/OGEX/stocks/FAC/orders"
    stubFor(post(urlEqualTo(path))
      .withHeader("X-Starfighter-Authorization", equalTo("dummy_auth")) // from config
      .withRequestBody(equalTo("{\"stock\":\"FAC\",\"price\":5100,\"direction\":\"buy\",\"qty\":100,\"account\":\"OGB12345\",\"orderType\":\"limit\",\"venue\":\"OGEX\"}"))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"A descriptive error message telling you that the order you attempted to place was invalid and not processed by the stock exchange.\"\n}")))


    val sut = StarFighterClient()

    // when
    val response = sut.placeOrderForStock("OGEX", "FAC", newOrder)

    // then
    whenReady(response) { r =>
      val expected = NewOrderResponse(ok = false, Left(ErrorMessage("A descriptive error message telling you that the order you attempted to place was invalid and not processed by the stock exchange.")))
      r should equal(expected)
    }
  }

  it should "post an order and parse sad 400 response for stocks on venue" in {
    // given
    val newOrder = NewOrder("OGB12345", "OGEX", "FAC", 5100, 100, Directions.Buy, OrderTypes.Limit)

    val path = "/venues/OGEX/stocks/FAC/orders"
    stubFor(post(urlEqualTo(path))
      .withHeader("X-Starfighter-Authorization", equalTo("dummy_auth")) // from config
      .withRequestBody(equalTo("{\"stock\":\"FAC\",\"price\":5100,\"direction\":\"buy\",\"qty\":100,\"account\":\"OGB12345\",\"orderType\":\"limit\",\"venue\":\"OGEX\"}"))
      .willReturn(
        aResponse()
          .withStatus(400)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"You provided symbol FOO and venue BAR in the JSON request but these did not match your URL.\"\n}")))


    val sut = StarFighterClient()

    // when
    val response = sut.placeOrderForStock("OGEX", "FAC", newOrder)

    // then
    whenReady(response) { r =>
      val expected = NewOrderResponse(ok = false, Left(ErrorMessage("You provided symbol FOO and venue BAR in the JSON request but these did not match your URL.")))
      r should equal(expected)
    }
  }

  it should "return a quote for a stock on a venue" in {
    // given
    val path = "/venues/OGEX/stocks/FAC/quote"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n    \"ok\": true,\n    \"symbol\": \"FAC\",\n    \"venue\": \"OGEX\",\n    \"bid\": 5100,\n    \"ask\": 5125,\n    \"bidSize\": 392,\n    \"askSize\": 711,\n    \"bidDepth\": 2748, \n    \"askDepth\": 2237, \n    \"last\": 5125, \n    \"lastSize\": 52, \n    \"lastTrade\": \"2015-07-13T05:38:17.33640392Z\", \n    \"quoteTime\": \"2015-07-13T05:38:17.33640392Z\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getQuoteForStock("OGEX", "FAC")

    // then
    whenReady(response) { r =>
      r.ok should be(right = true)
      val stockQuoteData = new StockQuoteData("FAC", "OGEX", Some(5100), Some(5125), Some(392), Some(711), Some(2748), Some(2237), Some(5125), Some(52), Some(new DateTime(2015, 7, 13, 5, 38,17, 336)), new DateTime(2015, 7, 13, 5, 38,17, 336))
      r.data.right.get should equal(stockQuoteData)
    }
  }

  it should "return a error when looking for stock quote with json error" in {
    // given
    val path = "/venues/OGEX/stocks/FAC/quote"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(404)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"symbol FOO does not exist on venue BAREX\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getQuoteForStock("OGEX", "FAC")

    // then
    whenReady(response) { r =>
      r.ok should be(right = false)
      r.data.left.get.msg should equal("symbol FOO does not exist on venue BAREX")
    }
  }

  it should "return a status for order of stock on venue" in {
    // given
    val path = "/venues/OGEX/stocks/FAC/orders/1"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"symbol\": \"ROBO\",\n  \"venue\": \"ROBUST\",\n  \"direction\": \"buy\",\n  \"originalQty\": 85,\n  \"qty\": 40,\n  \"price\": 993,\n  \"orderType\": \"immediate-or-cancel\",\n  \"id\": 1,\n  \"account\": \"FOO123\",\n  \"ts\": \"2015-08-10T16:10:32.987288+09:00\",\n  \"fills\": [\n    {\n      \"price\": 366,\n      \"qty\": 45,\n      \"ts\": \"2015-08-10T16:10:32.987292+09:00\"\n    }\n  ],\n  \"totalFilled\": 85,\n  \"open\": true\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getStatusForOrder("OGEX", "FAC", 1)

    // then
    whenReady(response) { r =>
      r.ok should be(right = true)
      val orderStatusData = new OrderStatusData("ROBO", "ROBUST", Directions.Buy, 85, 40, 993, OrderTypes.IoC, 1, "FOO123", new DateTime(2015, 8, 10, 16, 10, 32, 987), Seq(Fill(366, 45, new DateTime(2015, 8, 10, 16, 10, 32, 987))), 85, true)
      r.data.right.get should equal(orderStatusData)
    }
  }

  it should "return a unauthorized message when not allowed to check status of order" in {
    // given
    val path = "/venues/OGEX/stocks/FAC/orders/1"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(401)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"You are not authorized to view that order's details.\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getStatusForOrder("OGEX", "FAC", 1)

    // then
    whenReady(response) { r =>
      r.ok should be(right = false)
      r.data.left.get.msg should equal("You are not authorized to view that order's details.")
    }
  }

  it should "return a status for order of stock on venue when you cancel order happily" in {
    // given
    val path = "/venues/OGEX/stocks/FAC/orders/1"
    stubFor(delete(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"symbol\": \"ROBO\",\n  \"venue\": \"ROBUST\",\n  \"direction\": \"buy\",\n  \"originalQty\": 85,\n  \"qty\": 0,\n  \"price\": 993,\n  \"orderType\": \"immediate-or-cancel\",\n  \"id\": 1,\n  \"account\": \"FOO123\",\n  \"ts\": \"2015-08-10T16:10:32.987288+09:00\",\n  \"fills\": [\n    {\n      \"price\": 366,\n      \"qty\": 45,\n      \"ts\": \"2015-08-10T16:10:32.987292+09:00\"\n    }\n  ],\n  \"totalFilled\": 85,\n  \"open\": false\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.cancelOrder("OGEX", "FAC", 1)

    // then
    whenReady(response) { r =>
      r.ok should be(right = true)
      val orderStatusData = new OrderStatusData("ROBO", "ROBUST", Directions.Buy, 85, 0, 993, OrderTypes.IoC, 1, "FOO123", new DateTime(2015, 8, 10, 16, 10, 32, 987), Seq(Fill(366, 45, new DateTime(2015, 8, 10, 16, 10, 32, 987))), 85, false)
      r.data.right.get should equal(orderStatusData)
    }
  }

  it should "return a unauthorized message when not allowed to cancel order" in {
    // given
    val path = "/venues/OGEX/stocks/FAC/orders/1"
    stubFor(delete(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(401)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"Not authorized to delete that order.  You have to own account ABC123456.\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.cancelOrder("OGEX", "FAC", 1)

    // then
    whenReady(response) { r =>
      r.ok should be(right = false)
      r.data.left.get.msg should equal("Not authorized to delete that order.  You have to own account ABC123456.")
    }
  }

  it should "return a statueses for all orders on venue for a particular account" in {
    // given
    val path = "/venues/OGEX/accounts/FOO123/orders"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"venue\": \"ROBUST\",\n  \"orders\": [\n    {\n      \"symbol\": \"ROBO\",\n      \"venue\": \"ROBUST\",\n      \"direction\": \"buy\",\n      \"originalQty\": 85,\n      \"qty\": 40,\n      \"price\": 993,\n      \"orderType\": \"immediate-or-cancel\",\n      \"id\": 1,\n      \"account\": \"FOO123\",\n      \"ts\": \"2015-08-10T16:10:32.987288+09:00\",\n      \"fills\": [\n        {\n          \"price\": 366,\n          \"qty\": 45,\n          \"ts\": \"2015-08-10T16:10:32.987292+09:00\"\n        }\n      ],\n      \"totalFilled\": 85,\n      \"open\": true\n    }\n  ]\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getAllOrderStatuses("OGEX", "FOO123")

    // then
    whenReady(response) { r =>
      r.ok should be(right = true)
      val orderStatusData = new OrderStatusData("ROBO",
        "ROBUST",
        Directions.Buy,
        85,
        40,
        993,
        OrderTypes.IoC,
        1,
        "FOO123",
        new DateTime(2015, 8, 10, 16, 10, 32, 987),
        Seq(Fill(366, 45, new DateTime(2015, 8, 10, 16, 10, 32, 987))),
        85,
        true)

      val allOrderStatusesData = new AllOrderStatusesData("ROBUST", Seq[OrderStatusData](orderStatusData))
      r.data.right.get should equal(allOrderStatusesData)
    }
  }

  it should "return a unauthorized message when not allowed to check all status for account" in {
    // given
    val path = "/venues/OGEX/accounts/FOO123/orders"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(401)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"Not authorized to access details about that account's orders.\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getAllOrderStatuses("OGEX", "FOO123")

    // then
    whenReady(response) { r =>
      r.ok should be(right = false)
      r.data.left.get.msg should equal("Not authorized to access details about that account's orders.")
    }
  }

  it should "return a statuses for all orders on venue for a particular account for particular stock" in {
    // given
    val path = "/venues/OGEX/accounts/FOO123/stocks/FOO/orders"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody("{\n  \"ok\": true,\n  \"venue\": \"ROBUST\",\n  \"orders\": [\n    {\n      \"symbol\": \"ROBO\",\n      \"venue\": \"ROBUST\",\n      \"direction\": \"buy\",\n      \"originalQty\": 85,\n      \"qty\": 40,\n      \"price\": 993,\n      \"orderType\": \"immediate-or-cancel\",\n      \"id\": 1,\n      \"account\": \"FOO123\",\n      \"ts\": \"2015-08-10T16:10:32.987288+09:00\",\n      \"fills\": [\n        {\n          \"price\": 366,\n          \"qty\": 45,\n          \"ts\": \"2015-08-10T16:10:32.987292+09:00\"\n        }\n      ],\n      \"totalFilled\": 85,\n      \"open\": true\n    }\n  ]\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getAllOrderStatusesForStock("OGEX", "FOO", "FOO123")

    // then
    whenReady(response) { r =>
      r.ok should be(right = true)
      val orderStatusData = new OrderStatusData("ROBO",
        "ROBUST",
        Directions.Buy,
        85,
        40,
        993,
        OrderTypes.IoC,
        1,
        "FOO123",
        new DateTime(2015, 8, 10, 16, 10, 32, 987),
        Seq(Fill(366, 45, new DateTime(2015, 8, 10, 16, 10, 32, 987))),
        85,
        true)

      val allOrderStatusesData = new AllOrderStatusesData("ROBUST", Seq[OrderStatusData](orderStatusData))
      r.data.right.get should equal(allOrderStatusesData)
    }
  }

  it should "return a unauthorized message when not allowed to check all status for account for a stock" in {
    // given
    val path = "/venues/OGEX/accounts/FOO123/stocks/FOO/orders"
    stubFor(get(urlEqualTo(path))
      .willReturn(
        aResponse()
          .withStatus(401)
          .withBody("{\n  \"ok\": false,\n  \"error\": \"Not authorized to access details about that account's orders.\"\n}")))

    val sut = StarFighterClient()

    // when
    val response = sut.getAllOrderStatusesForStock("OGEX", "FOO",  "FOO123")

    // then
    whenReady(response) { r =>
      r.ok should be(right = false)
      r.data.left.get.msg should equal("Not authorized to access details about that account's orders.")
    }
  }

}
