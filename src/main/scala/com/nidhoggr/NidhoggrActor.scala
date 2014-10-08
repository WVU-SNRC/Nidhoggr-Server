package com.nidhoggr

import akka.actor.Actor
import spray.http.HttpResponse
import spray.http.MediaTypes.{`application/json`, `image/png`}
import spray.routing._
import spray.json._
import spray.json.DefaultJsonProtocol._
import java.io.{FileInputStream, File}
import java.nio.file.

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
                f.listFiles.filter(_.isDirectory).map(_.getName)
              }
              kvs.toJson.prettyPrint
            }
          }
        }
      } ~
      path("mrcbin" / "pick") {
        post {
          formFields("mrcfile") { (data) =>
            val f = new File(context.system.settings.config.getString("nidhoggr.mrcdir"), data)
            val fPath = f.getCanonicalPath
            val root = context.system.settings.config.getString("nidhoggr.mrcdir")
            if(fPath.substring(0, root.length) != root) {
              complete {
                HttpResponse(403, "Negative")
              }
            } else {
              respondWithMediaType(`image/png`) {
                complete {
                  val is = new FileInputStream(f)
                  val bytes = Stream.continually(is.read).takeWhile(-1 !=).map(_.toByte).toArray
                  HttpResponse(200, bytes)
                }
              }
            }
          }
        }
      }

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}