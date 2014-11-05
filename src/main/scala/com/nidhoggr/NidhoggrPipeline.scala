package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction, Task}
import scala.language.implicitConversions
import com.sksamuel.scrimage.filter.EdgeFilter
import com.sksamuel.scrimage.{Image => SKImage}

case class AccuracyBelowThresholdException(task: Task) extends RuntimeException



object NidhoggrPipeline {
  import NidhoggrPipeline.Image._

  def apply():NidhoggrPipeline = {
    new NidhoggrPipeline(List(expandInput(_), edgeDetection(_), axonOptimization(_), similarityCheck(_)))
  }

  //TODO all of those down there

  def normalize(msg:PipelineMsg): PipelineMsg = {
    def normalizer(img:Image):Image = {
      val sum = img.pixels.foldLeft(0.0)(_+_)
      Image(img.dimensions, img.pixels.map(_/sum))
    }
    PipelineMsg(msg.input.map((input) => (Trace(normalizer(input._1.image), input._1.coords), Image2Trace(normalizer(input._2.image)))), msg.task)
  }

  def expandInput(msg: PipelineMsg): PipelineMsg = {
      val res = for(
        input <- msg.input;
        coords <- input._1.coords;
        task <- msg.task
      ) yield {
        val h = Vector.fill(input._2.image.pixels.size - (coords._1 + input._1.image.dimensions._1))(0.0d) ++ input._1.image.pixels
        val expanded: Trace = Image(input._2.image.dimensions, (h ++ Vector.fill(input._2.image.pixels.size - h.size)(0.0d)).toArray)
        PipelineMsg(expanded, input._2, task)
      }
    res.getOrElse(msg)
  }

  def edgeDetection(msg: PipelineMsg): PipelineMsg = {
    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val image = input._2
      val trace = input._1
      val raster = Trace(image.image.filter(EdgeFilter), None)
      PipelineMsg(trace, raster, task)
    }
    res.getOrElse(msg)
  }


  def axonOptimization(msg: PipelineMsg): PipelineMsg = ???



  def similarityCheck(msg: PipelineMsg): PipelineMsg = {
//    def matrixGen(ref:Array[Double],obs:Array[Double]):Array[Double] = {
//      val J = for {p<-0 to ref.length;q<-0 to obs.length} yield (p,q)
//      J.par.map {case (a,b) => a-b}.toArray
//
//    }
    ???
  }

  case class Task(cell: String, file: String)
  case class Position (x:Int,y:Int)
  case class Velocity (u:Double,v:Double)
  case class Trace(image: Image, coords: Option[Coordinate])
  case class Image(dimensions: (Int, Int), pixels: Array[Double])
  object Image {
    implicit def Image2Trace(img: Image): Trace = Trace(img, None)
    implicit def SKImage2Image(img: SKImage): Image = Image((img.width, img.height), img.pixels.map(_.toDouble))
    implicit def Image2SKImage(img: Image): SKImage = SKImage(img.dimensions._1, img.dimensions._2, img.pixels.map(_.toInt))
  }
   case class PipelineMsg(input: Option[(Trace, Trace)], task: Option[Task])
  object PipelineMsg {
    def apply(trace: Trace, image: Trace, task: Task): PipelineMsg = {
      PipelineMsg(Some(trace, image), Some(task))
    }
  }

  def getTask(msg: PipelineMsg): Task = {
    msg.task.get
  }
  def getTrace(msg: PipelineMsg): Trace = {
    msg.input.get._1
  }

  def getImage(msg: PipelineMsg): Image = {
    getTrace(msg).image
  }

  type PipelineResult = (Option[NidhoggrPipeline], PipelineMsg)
  type Coordinate = (Int, Int)
  type PipelineFunction = PipelineMsg => PipelineMsg

}

class ImageVirtualAccessor(data: NidhoggrPipeline.Image){
  lazy val pix = data.pixels
  lazy val m = data.dimensions._1
  lazy val n = data.dimensions._2
  lazy val length = data.pixels.length
  def get(Row:Int)(Col:Int): Double = {
    pix.slice(Row*n,Row*n+n)(Col)
  }

  def set(Row: Int)(Col: Int)(Pix: Double): ImageVirtualAccessor = {
    val index = Col+(Row*n)
    new ImageVirtualAccessor(new NidhoggrPipeline.Image(data.dimensions,pix.updated(index,Pix)))
  }

}

class NidhoggrPipeline (pipe: List[PipelineFunction]) {
  def apply(msg: PipelineMsg): PipelineResult =
    pipe match {
      case (p::Nil)=> (None,p(msg))
      case (p::ps) => (Some(new NidhoggrPipeline(ps)),p(msg))
    }
}

