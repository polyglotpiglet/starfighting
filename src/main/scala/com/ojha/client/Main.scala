package com.ojha.client

import spray.json._
import spray.json.{JsValue, RootJsonFormat, DefaultJsonProtocol}

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by alexandra on 05/01/16.
 */
object Main extends App {

  case class Meow(venue: String, symbol: String)


  object MeowProtocol extends DefaultJsonProtocol {
    implicit val format = jsonFormat2(Meow)
  }

  import MeowProtocol._
  val meow: Meow = Meow("venue", "symbol")
  val rawr = "{\n  \"venue\": \"venue\",\n  \"symbol\": \"symbol\",\n \"cat\": \"felix\"\n}"
  val cat = rawr.parseJson
  val lalal = cat.convertTo[Meow]
  println(meow.toJson.prettyPrint)

}
