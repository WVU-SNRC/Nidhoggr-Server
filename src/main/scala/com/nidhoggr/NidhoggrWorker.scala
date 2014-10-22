package com.nidhoggr

import java.io.{File, FileInputStream}

import akka.actor.Actor
import com.nidhoggr.NidhoggrPipeline.{PipelineMsg, PipelineResult}
import com.nidhoggr.NidhoggrWorkLeader.{WorkResult, WorkFailed, WorkOrder}
import com.sksamuel.scrimage.io.TiffReader

import scala.annotation.tailrec

class NidhoggrWorker extends Actor {
  def receive = {
    case WorkOrder((trace, coordinate, task)) =>
      try {
        val root = context.system.settings.config.getString("nidhoggr.mrcdir")
        val tif = TiffReader.read(new FileInputStream(new File(root, task._1 + "/" + task._2)))
        val img = {
          (0 until tif.pixels.length).par.map{ i =>
            tif.pixels.slice(i * tif.width, (i * tif.width) + tif.width).map(_.toDouble)
          }
        }.toArray
        val res = runPipe((Some(NidhoggrPipeline()), (Some(((trace, Some(coordinate)), img)), Some(task))))
        sender ! WorkResult(res)
      } catch {
        case e @ AccuracyBelowThresholdException(currentTask) => sender ! WorkFailed(e, currentTask)
        case e: Exception => sender ! WorkFailed(e, task)
      }
  }

  @tailrec
  private def runPipe(pipe: PipelineResult): PipelineMsg = pipe match {
    case (Some(pipeline), msg) => runPipe(pipeline(msg))
    case (None, msg) => msg
  }
}
