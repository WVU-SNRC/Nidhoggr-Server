package com.nidhoggr

import java.io.{File, FileInputStream}

import akka.actor.Actor
import com.nidhoggr.NidhoggrPipeline.PipelineMsg
import com.nidhoggr.NidhoggrWorkLeader.{WorkFailed, WorkOrder, WorkResult}
import com.nidhoggr.NidhoggrPipeline.Image.{SKImage2Image, Image2Trace}
import com.sksamuel.scrimage.io.TiffReader

class NidhoggrWorker extends Actor {
  def receive = {
    case WorkOrder((trace, task)) =>
      try {
        val root = context.system.settings.config.getString("nidhoggr.mrcdir")
        val tif: NidhoggrPipeline.Image = TiffReader.read(new FileInputStream(new File(root, task.cell + "/" + task.file)))
        val res = NidhoggrPipeline.runPipe((Some(NidhoggrPipeline()), PipelineMsg(trace, tif, task)), 0)
        sender ! WorkResult(res)
      } catch {
        case e @ AccuracyBelowThresholdException(currentTask) => sender ! WorkFailed(e, currentTask)
        case e: Exception => sender ! WorkFailed(e, task)
      }
  }


}
