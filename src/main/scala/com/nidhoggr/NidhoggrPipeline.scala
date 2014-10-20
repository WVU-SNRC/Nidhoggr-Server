package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction, Task}

object NidhoggrPipeline {

  def apply():NidhoggrPipeline = {
    new NidhoggrPipeline(List(expandInput(_), edgeDetection(_), axonOptimization(_), earthMoversDistance(_)))
  }

  //TODO all of those down there

  def expandInput(msg: PipelineMsg): PipelineMsg = ???

  def edgeDetection(msg: PipelineMsg): PipelineMsg = ???

  def axonOptimization(msg: PipelineMsg): PipelineMsg = ???

  def earthMoversDistance(msg: PipelineMsg): PipelineMsg = ???

  type Task = (String, String) //Why is this Strings?
  type Image = Array[Array[Int]]
  type PipelineMsg = (Option[(Image, Image)], Option[Task])
  type PipelineFunction = PipelineMsg => PipelineMsg
  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
}

case class AccuracyBelowThresholdException(task: Task) extends RuntimeException

class NidhoggrPipeline (pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult =
    pipe match {
      case (p::Nil)=> (None,p(msg))
      case (p::ps) => (Some(new NidhoggrPipeline(ps)),p(msg))
    }
}

object AxonPipeline {
  def apply() = {
    val functions = List(expandInput(_),edgeDetection(_),axonOptimization(_),earthMoversDistance(_))
    new NidhoggrPipeline(functions)
  }

  //TODO all of those down there

  def expandInput(msg: PipelineMsg): PipelineMsg = ???

  def edgeDetection(msg: PipelineMsg): PipelineMsg = ???

  def axonOptimization(msg: PipelineMsg): PipelineMsg = ???

  def earthMoversDistance(msg: PipelineMsg): PipelineMsg = ???
}
