package com.nidhoggr

import akka.actor.{Actor, ActorRef, Props, Terminated}
import akka.routing.{ActorRefRoutee, BroadcastRoutingLogic, Router}
import com.nidhoggr.NidhoggrPipeline._
import com.nidhoggr.NidhoggrWorkLeader._

import scala.collection.mutable

class NidhoggrWorkLeader extends Actor {
  val taskQueue = new mutable.Queue[NewTask]()
  val workMap = mutable.Map.empty[ActorRef, NewTask]
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
        val task = taskQueue.dequeue()
        workMap += (sender -> task)
        sender ! WorkOrder(task)
      } else {
        sender ! NoWork
      }
    case WorkFailed(e, currentTask) =>
      log("%s failed task %s".format(sender(), workMap(sender())))
      e match {
        case ex: AccuracyBelowThresholdException =>
          saveTask(currentTask)
        case ex => ???
      }
    case Terminated(actor) =>
      log("%s died while processing %s".format(sender(), workMap(sender())._2))
    case WorkResult(res) =>
      log("%s completed task %s".format(sender(), workMap(sender())._2))
      //TODO: Convert completed tiff stack to imod
  }

  def saveTask(t: Task) = ???

  def log(msg: String) = ???
}


object NidhoggrWorkLeader {
  type NewTask = (Trace, Task)
  case class NewWork(task: NewTask)
  case class WorkOrder(task: NewTask)
  case class WorkFailed(e: Exception, task: Task)
  case class WorkResult(res: PipelineMsg)
  case object GetWork
  case object WorkReady
  case object NoWork
}