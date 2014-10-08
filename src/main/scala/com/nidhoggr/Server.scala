package com.nidhoggr

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http

import scala.concurrent.duration._

object Server extends App {
  implicit val system = ActorSystem("on-spray-can")
  val service = system.actorOf(Props[NidhoggrActor], "nidhoggr-serve")

  implicit val timeout = Timeout(5.seconds)
  IO(Http) ? Http.Bind(service, interface="0.0.0.0", port=8086)
}