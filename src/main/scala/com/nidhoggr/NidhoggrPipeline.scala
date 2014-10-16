package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction}

object NidhoggrPipeline {
  type Task = (String, String) //Why is this Strings?
  type Image = List[List[Int]]
  type PipelineMsg = (Option[(Image, Image)], Task)
  type PipelineFunction = PipelineMsg => PipelineMsg
  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
}

class AccuracyBelowThresholdException(msg:String) extends RuntimeException

class NidhoggrPipeline private (pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult = try{
    pipe match{
      case (p::ps) => (Some(new NidhoggrPipeline(ps)),p(msg))
      case (p::Nil)=> (None,p(msg))
    }
  }catch{
    case e: AccuracyBelowThresholdException => try{
      ???   //TODO try to expand the uh field o' view
    }catch{
      case e:AccuracyBelowThresholdException => ???  //TODO put T back on the stack
    }
  }
}

object AxonPipeline extends NidhoggrPipeline{
  def apply() = {
    val functions = List(edgeDetection(_),axonOptimization(_),earthMoversDistance(_))
    new NidhoggrPipeline(functions)
  }

  //TODO all of those down there

  def edgeDetection(msg:PipelineMsg):PipelineMsg = ???

  def axonOptimization(msg:PipelineMsg):PipelineMsg = ???

  def earthMoversDistance(msg:PipelineMsg):PipelineMsg = ???
}