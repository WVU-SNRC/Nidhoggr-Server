package com.nidhoggr

import akka.actor.{Props, Actor}
import akka.routing.{ActorRefRoutee, BroadcastRoutingLogic, Router}
import com.nidhoggr.NidhoggrPipeline.PipelineMsg
import com.nidhoggr.NidhoggrWorkLeader._

import scala.collection.mutable

class NidhoggrWorkLeader extends Actor {
  val taskQueue = new mutable.Queue[PipelineMsg]()
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
  }
}


object NidhoggrWorkLeader {
  case class NewWork(task: PipelineMsg)
  case class WorkOrder(task: PipelineMsg)
  case object GetWork
  case object WorkReady
  case object NoWork
}