package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction}

class AccuracyBelowThresholdException(msg:String) extends RuntimeException

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
    }
    val x = measure(normalize(msg))
    if (x > ???) msg
    else throw new AccuracyBelowThresholdException("Below Accuracy Threshold")
  }

  type Task = (String, String) //Why is this Strings?
  type Image = Array[Array[Int]]
  type PipelineMsg = (Option[(Image, Image)], Option[Task])
  type PipelineFunction = PipelineMsg => PipelineMsg
  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
}



class NidhoggrPipeline (pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult = try{
    pipe match {
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
