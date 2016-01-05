package com.ojha.client

import com.typesafe.config.ConfigFactory

/**
 * Created by alexandra on 04/01/16.
 */
trait Configurable {
  lazy val config = ConfigFactory.load()
}
