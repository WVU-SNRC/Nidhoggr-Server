package com.nidhoggr

import java.io.{File, FileInputStream}

import akka.actor.Actor
import com.nidhoggr.NidhoggrPipeline.PipelineMsg
import com.nidhoggr.NidhoggrWorkLeader.{WorkFailed, WorkOrder, WorkResult}
import com.nidhoggr.NidhoggrPipeline.Image.{SKImage2Image, Image2Trace}
import com.sksamuel.scrimage.io.TiffReader

class NidhoggrWorker extends Actor {
  def receive = {
    case WorkOrder((trace, centroids, task, params)) =>
      try {
        val root = context.system.settings.config.getString("nidhoggr.mrcdir")
        val tif: NidhoggrPipeline.Image = TiffReader.read(new FileInputStream(new File(root, task.cell + "/" + task.file)))
        val c1 = (trace.map(_._1).foldLeft(0)(_ + _) / trace.length, trace.map(_._2).foldLeft(0)(_ + _) / trace.length)
        val res = NidhoggrPipeline.runPipe((Some(NidhoggrPipeline()), PipelineMsg(trace, centroids.+:(c1), tif, task, params.getOrElse(NidhoggrPipeline.distCoff, NidhoggrPipeline.convCoff, NidhoggrPipeline.imgCoff))), 0)
        sender ! WorkResult(res)
      } catch {
        case e: Exception => sender ! WorkFailed(e, task)
      }
  }
}
