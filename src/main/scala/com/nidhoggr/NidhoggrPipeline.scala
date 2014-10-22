package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction, Task}
import scala.language.implicitConversions

case class AccuracyBelowThresholdException(task: Task) extends RuntimeException

object NidhoggrPipeline {
  implicit def Image2Trace(img: Image) = Trace(img, None)

  def apply():NidhoggrPipeline = {
    new NidhoggrPipeline(List(expandInput(_), edgeDetection(_), axonOptimization(_), binSimilarityDistance(_)))
  }

  //TODO all of those down there

  def normalize(msg:PipelineMsg): PipelineMsg = {
    def normalizer(img:Image) = {
      val sum = img.pixels.foldLeft(0.0)(_+_)
      Image(img.dimensions, img.pixels.map(_/sum))
    }
    PipelineMsg(msg.input.map((input) => (Trace(normalizer(input._1.image), input._1.coords), input._2)), msg.task)
  }

  def expandInput(msg: PipelineMsg): PipelineMsg = {
      val res = for(
        input <- msg.input;
        coords <- input._1.coords;
        task <- msg.task
      ) yield {
        val h = Vector.fill(input._2.pixels.size - (coords._1 + input._1.image.dimensions._1))(0.0d) ++ input._1.image.pixels
        val expanded: Trace = Image(input._2.dimensions, (h ++ Vector.fill(input._2.pixels.size - h.size)(0.0d)).toArray)
        PipelineMsg(expanded, input._2, task)
      }
    res.getOrElse(msg)
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

  case class Task(cell: String, file: String)
  case class Trace(image: Image, coords: Option[Coordinate])
  case class Image(dimensions: (Int, Int), pixels: Array[Double])
  case class PipelineMsg(input: Option[(Trace, Image)], task: Option[Task])
  object PipelineMsg {
    def apply(trace: Trace, image: Image, task: Task): PipelineMsg = {
      PipelineMsg(Some(trace, image), Some(task))
    }
  }
  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
  type Coordinate = (Int, Int)
  type PipelineFunction = PipelineMsg => PipelineMsg
}

class NidhoggrPipeline (pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult =
    pipe match {
      case (p::Nil)=> (None,p(msg))
      case (p::ps) => (Some(new NidhoggrPipeline(ps)),p(msg))
    }
}

