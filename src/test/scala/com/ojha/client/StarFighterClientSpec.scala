package com.ojha.client


import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration._
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import org.scalatest.concurrent.ScalaFutures

import spray.json._

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
    val response = sut.heartBeat()

    // then
    whenReady(response) { r =>
      r should equal(VanillaResponse(ok = true, Some("")))
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
      r should equal(VenueResponse(ok = true, None, Some("TESTEX")))
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
      r should equal(VenueResponse(ok = false, Some("The venue appears to be non-responsive even though its server is up."), None))
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
      r should equal(VenueResponse(ok = false, Some("No venue exists with the symbol TESTEX."), None))
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
