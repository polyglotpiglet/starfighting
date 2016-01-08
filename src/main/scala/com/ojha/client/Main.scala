package com.ojha.client

import org.joda.time.DateTime
import spray.json._
import spray.json.{JsValue, RootJsonFormat, DefaultJsonProtocol}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
 * Created by alexandra on 05/01/16.
 */
object Main extends App {

  val client = StarFighterClient()

  val response = client.orderBookForStockOnVenue("TESTEX", "FOOBAR")
  response onComplete {
    case Success(r) =>
      println(r)
    case Failure(e) =>
      println(e)
  }

}
