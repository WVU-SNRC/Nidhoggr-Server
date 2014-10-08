package com.nidhoggr

import akka.actor.{ActorLogging, Actor}

object NidhoggrPipeline {
  case class Tracker(cell: String, seed: Array[Array[Boolean]])
}

class NidhoggrPipeline extends Actor with ActorLogging {
  override def receive = {
    case _ =>
      log.info("Not implemented")
  }
}
