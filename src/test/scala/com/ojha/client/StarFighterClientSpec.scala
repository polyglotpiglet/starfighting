package com.ojha.client


import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration._
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

      val res = r.asInstanceOf[StocksInfoResponse]
      res.ok should be(right = true)

      val symbols = res.symbols.get
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
      val res = r.asInstanceOf[StarFighterError]
      res.ok should be(right = false)
      res.error should equal(Some("No venue exists with the symbol OGEX"))
    }
  }


  ignore should "post an order to starfighter" in {
    // given
    val sut = StarFighterClient()
    val order = Order("act", "HPYEX", "IBM", 30000, 100, Directions.Buy, OrderTypes.Limit)

    // when
    sut.execute(order)

    // then
    verify(postRequestedFor(urlEqualTo("/ob/api"))
      .withHeader("X-Starfighter-Authorization", equalTo("dummy_auth"))
      .withRequestBody(containing("lalala")))
  }


}
