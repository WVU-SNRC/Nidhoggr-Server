package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction, Task}

case class AccuracyBelowThresholdException(task: Task) extends RuntimeException

object NidhoggrPipeline {

  def apply():NidhoggrPipeline = {
    new NidhoggrPipeline(List(expandInput(_), edgeDetection(_), axonOptimization(_), binSimilarityDistance(_)))
  }

  //TODO all of those down there

  def normalize(msg:PipelineMsg): PipelineMsg = {
    def normalizer(img:Image) = {
      val sum = img.flatten.foldLeft(0.0)(_+_)
      img.map(_.map(_/sum))
    }

    (Some(((normalizer(msg._1.get._1._1), None),normalizer(msg._1.get._2))),msg._2)
  }

  def expandInput(msg: PipelineMsg): PipelineMsg = {
    val expanded = for(
      input <- msg._1;
      coords <- input._1._2;
    ) yield {
      val width = input._2.size
      val height = input._2(0).size
      for(y <- 0 to height) yield {
        if(y < coords._2) {
          Vector.fill(width)(0).toArray
        }
        else {
          Vector.fill(coords._1)(0).toArray ++ input._1._1(y).map(_.toInt) ++ Vector.fill(width - (coords._1 + input._1._1(y).size))(0).toArray
        }
      }
    }
    ???
  }

  def edgeDetection(msg: PipelineMsg): PipelineMsg = ???

  def axonOptimization(msg: PipelineMsg): PipelineMsg = ???

  def binSimilarityDistance(msg: PipelineMsg): PipelineMsg = {
//    def measure(ref:Image,obs:Image):Double = {
//      val Ip = (for (i <- List.range(0, ref.length)) yield i).map(a=> (for (j <- List.range(0, ref(0).length)) yield j).map(b=>(a,b))).flatten
//      val Iq = (for (i <- List.range(0, obs.length)) yield i).map(a=> (for (j <- List.range(0, obs(0).length)) yield j).map(b=>(a,b))).flatten
//      val J = for {p <- Ip;q<-Iq} yield(p,q)
//      type Index = ((Int,Int),(Int,Int))
//      def computeSimilarityMatrix(indexer:List[Index],ref:Image,obs:Image):List[Double] = indexer match{ //TODO discuss whether we want to replace this with flow for emd
//        case ((ref1,ref2),(obs1,obs2))::is => List((ref(ref1)(ref2) - obs(obs1)(obs2))) ++ computeSimilarityMatrix(is,ref,obs)
//        case Nil => Nil
//      }
//      ???
//    }
//    val normed = normalize(msg)
//    val x = measure(???,???) // or just make sure it comes first in the pipeline
//    if (x > ???) msg
//    else throw new AccuracyBelowThresholdException(msg._2.get)
    ???
  }

  type Task = (String, String)
  type Trace = (Image, Option[Coordinate])
  type Image = Array[Array[Double]]
  type PipelineMsg = (Option[(Trace, Image)], Option[Task])
  type PipelineFunction = PipelineMsg => PipelineMsg
  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
  type Coordinate = (Int, Int)
}

class NidhoggrPipeline (pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult =
    pipe match {
      case (p::Nil)=> (None,p(msg))
      case (p::ps) => (Some(new NidhoggrPipeline(ps)),p(msg))
    }
}

