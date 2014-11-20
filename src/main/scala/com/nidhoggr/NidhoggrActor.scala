package com.nidhoggr

import java.io.{File, FileInputStream}

import akka.actor.{Actor, Props}
import com.nidhoggr.NidhoggrPipeline.{Task, Image, Trace}
import com.nidhoggr.NidhoggrWorkLeader.NewWork
import com.sksamuel.scrimage.Format
import com.sksamuel.scrimage.io.TiffReader
import spray.http.HttpResponse
import spray.http.MediaTypes.{`application/json`, `image/png`}
import spray.json.DefaultJsonProtocol._
import spray.json._
import spray.routing._

class NidhoggrActor extends Actor with HttpService {
  val workLeader = actorRefFactory.actorOf(Props[NidhoggrWorkLeader])

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
          val f = new File(context.system.settings.config.getString("nidhoggr.mrcdir"), data + "/p.0000.tif")
          val fPath = f.getCanonicalPath
          val root = context.system.settings.config.getString("nidhoggr.mrcdir")
          if(fPath.substring(0, root.length) != root) {
            complete {
              HttpResponse(403, "Negative")
            }
          } else {
            respondWithMediaType(`image/png`) {
              complete {
                val img = TiffReader.read(new FileInputStream(f))
                HttpResponse(200, img.writer(Format.PNG).write())
              }
            }
          }
        }
      }
    } ~
    path("mrcbin" / "next") {
      post {
        formFields("mrcfile", "index") { (data: String, index: String) =>
          val f = new File(context.system.settings.config.getString("nidhoggr.mrcdir"), s"$data/p.$index.tif")
          val fPath = f.getCanonicalPath
          val root = context.system.settings.config.getString("nidhoggr.mrcdir")
          if(fPath.substring(0, root.length) != root) {
            complete {
              HttpResponse(403, "Negative")
            }
          } else {
            respondWithMediaType(`image/png`) {
              complete {
                val img = TiffReader.read(new FileInputStream(f))
                HttpResponse(200, img.writer(Format.PNG).write())
              }
            }
          }
        }
      }
    } ~
    path("trace" / "submit") {
      post {
        formFields("cell", "coords", "trace", "centroids"){
          (cell, coords, trace, centroids) =>
            val cSplit = cell.split("/", 2)
            val root = context.system.settings.config.getString("nidhoggr.mrcdir")
            val f = new File(root, cell)
            if(f.getCanonicalPath.substring(0, root.length) != root)
              complete(HttpResponse(403, "Negative"))
            else {
              val trace = cell.parseJson.convertTo[Array[(Int, Int)]]
              val cents = centroids.parseJson.convertTo[Array[(Int, Int)]]
              workLeader ! NewWork(trace, cents, Task(cSplit(0), cSplit(1)), None)
              complete(HttpResponse(202, "Accepted"))
            }
        }
      }
    }

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}