package com.nidhoggr

import akka.actor.Actor
import com.nidhoggr.NidhoggrPipeline.{PipelineMsg, PipelineResult}
import com.nidhoggr.NidhoggrWorkLeader.{WorkResult, WorkAck, WorkFailed, WorkOrder}

import scala.annotation.tailrec

class NidhoggrWorker extends Actor {
  def receive = {
    case WorkOrder(result) =>
      sender ! WorkAck
      try {
        val res = runPipe(result)
        sender ! WorkResult(res)
      } catch {
        case e: Exception => sender ! WorkFailed(e)
      }
  }

  @tailrec
  private def runPipe(pipe: PipelineResult): PipelineMsg = pipe match {
    case (Some(pipeline), msg) => runPipe(pipeline(msg))
    case (None, msg) => msg
  }
}
