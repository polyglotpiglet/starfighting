package com.ojha.client


import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration._
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import org.scalatest.concurrent.ScalaFutures

import com.ojha.client.ResponseProtocol._
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
          .withBody(Response(ok = true, "").toJson.compactPrint)))

    val sut = StarFighterClient()

    // when
    val response = sut.heartBeat()

    // then
    whenReady(response) { r =>
      r should equal(Response(ok = true, ""))
    }
  }


  it should "post an order to starfighter" in {
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
