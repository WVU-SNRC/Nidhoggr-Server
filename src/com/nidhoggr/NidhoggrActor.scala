package com.nidhoggr

import akka.actor.Actor
import spray.http.MediaTypes.`application/json`
import spray.routing._
import spray.json._
import spray.json.DefaultJsonProtocol._
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
            val f = new File(context.system.settings.config.getString("nidhoggr.mrcdir"))
            val kvs = {
              f.listFiles.filter(_.isDirectory)
                .map((file : File) => (file.getName, file.listFiles))
                .filter({ case (name : String, null) => false case default => true })
                .map({ case (name : String, files : Array[File]) => (name, files.filter(_.isFile).map(_.getName))})
            }
            Map(kvs:_*).toJson.prettyPrint
          }
        }
      }
    }

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}
