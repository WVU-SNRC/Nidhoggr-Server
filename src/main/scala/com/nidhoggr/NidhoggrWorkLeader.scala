package com.nidhoggr

import akka.actor.{Actor, Props}
import akka.routing.{ActorRefRoutee, BroadcastRoutingLogic, Router}
import com.nidhoggr.NidhoggrPipeline.{PipelineMsg, PipelineResult}
import com.nidhoggr.NidhoggrWorkLeader._

import scala.collection.mutable

class NidhoggrWorkLeader extends Actor {
  val taskQueue = new mutable.Queue[PipelineResult]()
  val router = Router(BroadcastRoutingLogic(), Vector.fill(5){
    ActorRefRoutee(context.actorOf(Props[NidhoggrWorker]))
  })

  def receive = {
    case NewWork(task) =>
      if(taskQueue.isEmpty) {
        taskQueue.enqueue(task)
        router.route(WorkReady, self)
      } else {
        taskQueue.enqueue(task)
      }
    case GetWork =>
      if(taskQueue.nonEmpty) {
        sender ! WorkOrder(taskQueue.dequeue())
      } else {
        sender ! NoWork
      }
    case WorkFailed(e) => ???
  }
}


object NidhoggrWorkLeader {
  case class NewWork(task: PipelineResult)
  case class WorkOrder(task: PipelineResult)
  case class WorkFailed(e: Exception)
  case class WorkResult(res: PipelineMsg)
  case object GetWork
  case object WorkReady
  case object NoWork
  case object WorkAck
}