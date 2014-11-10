package com.nidhoggr

import com.nidhoggr.NidhoggrPipeline.{PipelineResult, PipelineMsg, PipelineFunction, Task}
import scala.annotation.tailrec
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
      val min = img.pixels.min
      val max = img.pixels.max
      //Magic numbers below are actually integer representations of Tiff values for black and white.
      //First byte is Alpha, followed by RGB
      //-16777216 produces 255 0 0 0 as bytes, meaning black
      //-1 produces 255 255 255 255, meaning white
      Image(img.dimensions, img.pixels.map((pix: Double) => (pix - min) * ((-1 - -16777216) / (max - min)) + -16777216))
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


  def axonOptimization(msg: PipelineMsg): PipelineMsg = {
    def distance(p: Coordinate, q: Coordinate) = {
      math.sqrt(math.pow(p._1 - q._1, 2) + math.pow(p._2 - q._2, 2))
    }
    def angle(p: Coordinate, q: Coordinate) = {
      val product = (p._1 * q._1) + (p._2 * q._2)
      product / (math.sqrt(math.pow(p._1, 2) + math.pow(p._2, 2)) * math.sqrt(math.pow(q._1, 2) + math.pow(q._2, 2)))
    }

    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      @tailrec
      def iterContour(contour: Array[Coordinate], energy: Int): Array[Coordinate] = {
        def pointEnergy(i: Int, p: Coordinate): Int = {
          val left = contour(i - 1)
          val right = contour(i + 1)
          math.round((1 * (distance(left, p) + distance(p, right))) + (1 * (angle(left, p) + angle(p, right))) + (1 * input._2.image.get(contour(i)._1)(contour(i)._2))).toInt
        }

        @tailrec
        def iterPoint(i: Int)(p: Coordinate, e: Int): (Coordinate, Int) = {
          val possible: List[(Coordinate, Int)] = List(
            (p._1 + 1, p._2 + 1),
            (p._1 + 1, p._2),
            (p._1 + 1, p._2 - 1),
            (p._1, p._2 + 1),
            (p._1, p._2 - 1),
            (p._1 - 1, p._2 + 1),
            (p._1 - 1, p._2),
            (p._1 - 1, p._2 - 1)
          ).map((c: Coordinate) => (c, pointEnergy(i, c))).filter(_._2 < e).sortBy(_._2)
          possible match {
            case n :: ns => iterPoint(i)(n._1, n._2)
            case Nil => (p, e)
          }
        }


        val pointEnergies = for (i <- 0 until contour.size) yield {
          iterPoint(i)(contour(i), pointEnergy(i, contour(i)))
        }
        val E: Int = pointEnergies.map(_._2).foldLeft(0)((a: Int, b: Int) => a + b)
        if (E < energy)
          iterContour(pointEnergies.map(_._1).toArray, E)
        else
          contour
      }
      val contour = input._1.image.pixels.zipWithIndex.filter(_._1 > 0).filter(_._2 % 2 == 0).map{ case (value: Double, index: Int) =>
        val row = index % input._1.image.dimensions._1
        val col = (row * input._1.image.dimensions._1) % input._1.image.dimensions._2
        (row, col)
      }

      def contour2Image(contour: Array[Coordinate]): Image = {
        var img: ImageVirtualAccessor = Image((input._1.image.m, input._1.image.n), Vector.fill(input._1.image.m, input._1.image.n)(0d).flatten.toArray)
        for((m, n) <- contour) {
          img = img.set(m)(n)(1d)
        }
        Image((input._1.image.m, input._1.image.n), img.pix)
      }
      PipelineMsg(input._1, contour2Image(iterContour(contour, Int.MaxValue)), task)
    }
    res.get
  }



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
    implicit def Image2ImageVirtualAccessor(img: Image): ImageVirtualAccessor = new ImageVirtualAccessor(img)
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

