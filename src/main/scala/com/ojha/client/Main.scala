package com.ojha.client

import com.ojha.tradingstrategies.{SellSide, ChockABlock}

/**
 * Created by alexandra on 05/01/16.
 */
object Main extends App {

  val strategy = new SellSide()
  strategy.execute()
}
