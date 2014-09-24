package com.nidhoggr

import akka.actor.Actor
import spray.http.MediaTypes.`application/json`
import spray.routing._

class NidhoggrActor extends Actor with HttpService {
  val myRoute =
    path("") {
      get {
        complete {
          "The Nidhoggr Project."
        }
      }
    } ~
    path("mrcbin" / "list") {
      get {
        respondWithMediaType(`application/json`) {
          complete("Hello foo")
        }
      }
    }

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}
