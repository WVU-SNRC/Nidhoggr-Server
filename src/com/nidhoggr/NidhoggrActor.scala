package com.nidhoggr

import akka.actor.Actor
import spray.http.MediaTypes.`application/json`
import spray.routing._
import java.io.File

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
          complete {
            val f = new File("/home/calvr")
            "[" + f.listFiles.filter(_.isFile).map(_.getName).mkString(", ") + "]"
          }
        }
      }
    }

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}
