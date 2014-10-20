package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction, Task}

case class AccuracyBelowThresholdException(task: Task) extends RuntimeException

object NidhoggrPipeline {

  def apply():NidhoggrPipeline = {
    new NidhoggrPipeline(List(expandInput(_), edgeDetection(_), axonOptimization(_), earthMoversDistance(_)))
  }

  //TODO all of those down there

  def normalize(msg:PipelineMsg): PipelineMsg = ???

  def expandInput(msg: PipelineMsg): PipelineMsg = ???

  def edgeDetection(msg: PipelineMsg): PipelineMsg = ???

  def axonOptimization(msg: PipelineMsg): PipelineMsg = ???

  def earthMoversDistance(msg: PipelineMsg): PipelineMsg = {
    def measure(msg: PipelineMsg): Double = {
      val P = msg._1.get._1
      val IsubP = (for (i <- List.range(0, P.length)) yield i).map(a=> (for (j <- List.range(0, P(0).length)) yield j).map(b=>(a,b)))
      val IsubQ = List((0,0)) //TODO Complicate task by adding a reference to the producing countour image
      val J = IsubP.zip(IsubQ)
      3.4  //I think we all know this is a placeholder
    }
    val x = measure(normalize(msg))
    if (x > ???) msg
    else throw new AccuracyBelowThresholdException(msg._2.get)
  }

  type Task = (String, String) //Why is this Strings?
  type Image = Array[Array[Int]]
  type PipelineMsg = (Option[(Image, Image)], Option[Task])
  type PipelineFunction = PipelineMsg => PipelineMsg
  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
}

class NidhoggrPipeline (pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult =
    pipe match {
      case (p::Nil)=> (None,p(msg))
      case (p::ps) => (Some(new NidhoggrPipeline(ps)),p(msg))
    }
}

