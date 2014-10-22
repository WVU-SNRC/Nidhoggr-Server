package com.nidhoggr

import java.io.{File, FileInputStream}

import akka.actor.Actor
import com.nidhoggr.NidhoggrPipeline.{Image, Trace, PipelineMsg, PipelineResult}
import com.nidhoggr.NidhoggrWorkLeader.{WorkResult, WorkFailed, WorkOrder}
import com.sksamuel.scrimage.io.TiffReader

import scala.annotation.tailrec

class NidhoggrWorker extends Actor {
  def receive = {
    case WorkOrder((trace, task)) =>
      try {
        val root = context.system.settings.config.getString("nidhoggr.mrcdir")
        val tif = TiffReader.read(new FileInputStream(new File(root, task.cell + "/" + task.file)))
        val img = Image((tif.width, tif.height), tif.pixels.map(_.toDouble))
        val res = runPipe((Some(NidhoggrPipeline()), PipelineMsg(Some(trace, img), Some(task))))
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
