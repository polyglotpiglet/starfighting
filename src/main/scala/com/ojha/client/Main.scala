package com.ojha.client

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by alexandra on 05/01/16.
 */
object Main extends App {

  val client = StarFighterClient()
  val response = client.heartBeat()
  response.onSuccess { case r =>
    println(r.toString)
  }

  client.shutdown()

}
