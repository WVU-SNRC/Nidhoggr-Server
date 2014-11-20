package com.nidhoggr

import java.io.File

import com.nidhoggr.NidhoggrPipeline._
import scala.annotation.tailrec
import scala.language.implicitConversions
import com.sksamuel.scrimage.filter.{InvertFilter, GaussianBlurFilter, EdgeFilter}
import com.sksamuel.scrimage.{Image => SKImage}

import scala.util.Try

case class AccuracyBelowThresholdException(task: Task) extends RuntimeException


object NidhoggrPipeline {
  val BLACK = -16777216
  val WHITE = -1
  val distCoff = -7d
  val convCoff = 0.7
  val imgCoff = -1d
  import NidhoggrPipeline.Image._

  def apply(): NidhoggrPipeline = {
    new NidhoggrPipeline(List(normalize, centralize, gaussianBlur, edgeDetection, axonOptimization))
  }

  @tailrec
  def runPipe(pipe: PipelineResult, i: Int): PipelineMsg = pipe match {
    case (Some(pipeline), msg) =>
      runPipe(pipeline(msg), i + 1)
    case (None, msg) =>
      for(input <- msg.input) {
        val distCoff = msg.parameters._1
        val convCoff = msg.parameters._2
        val imgCoff = msg.parameters._3
        val output: SKImage = input._2.image
        output.write(new File(s"output-$distCoff-$convCoff-$imgCoff.png"))
      }
      msg
  }

  //TODO all of those down there

  def centralize(msg: PipelineMsg): PipelineMsg = {
    val res = for(input <- msg.input; task <- msg.task) yield {
      val p = input._3.head
      val q = input._3(1)
      val xoff = p._1 - q._1
      val yoff = p._2 - q._2
      PipelineMsg(input._1.map(c => (c._1 - xoff, c._2 - yoff)), input._3.tail, input._2, task, msg.parameters)
    }
    res.getOrElse(msg)
  }

  def normalize(msg: PipelineMsg): PipelineMsg = {
    def normalizer(img: SKImage):Image = {
      val min = img.pixels.min
      val max = img.pixels.max
      //Magic numbers below are actually integer representations of Tiff values for black and white.
      //First byte is Alpha, followed by RGB
      //-16777216 produces 255 0 0 0 as bytes, meaning black
      //-1 produces 255 255 255 255, meaning white
      Image(img.dimensions, img.pixels.map((pix: Int) => ((pix - min) * ((WHITE - BLACK) / (max - min))) + BLACK))
    }
    PipelineMsg(msg.input.map((input) => (input._1, normalizer(input._2.image), input._3)), msg.task, msg.parameters)
  }

  def edgeDetection(msg: PipelineMsg): PipelineMsg = {
    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val image = input._2
      val trace = input._1
      val raster = image.image.filter(EdgeFilter)
      PipelineMsg(trace, input._3, raster, task, msg.parameters)
    }
    res.getOrElse(msg)
  }


  def gaussianBlur(msg: PipelineMsg): PipelineMsg = {
    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val image = input._2
      val trace = input._1
      val raster = image.image.filter(GaussianBlurFilter())
      PipelineMsg(trace, input._3, raster, task, msg.parameters)
    }
    res.getOrElse(msg)
  }

  def invertImage(msg: PipelineMsg): PipelineMsg = {
    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val image = input._2
      val trace = input._1
      val raster = image.image.filter(InvertFilter)
      PipelineMsg(trace, input._3, raster, task, msg.parameters)
    }
    res.getOrElse(msg)
  }

  def axonOptimization(msg: PipelineMsg): PipelineMsg = {
    def dist(p: Coordinate, q: Coordinate) = {
      math.sqrt(math.pow(p._1 - q._1, 2) + math.pow(p._2 - q._2, 2))
    }
    def angle(left: Coordinate, middle: Coordinate, right: Coordinate) = {
      //val ang = math.toDegrees(math.acos((math.pow(dist(middle, left), 2) + math.pow(dist(middle, right), 2) - math.pow(dist(left, right), 2))
      //  / (2 * dist(middle, left) * dist(middle, right))))
      val tx = left._1 - 2 * middle._1 + right._1
      val ty = left._2 - 2 * middle._2 + right._2
      val ang = math.pow(tx, 2) + math.pow(ty, 2)
      //println(s"Angle: $ang for point: $left $middle $right")
      if(ang.isNaN){
        Double.MaxValue
      }
      else
        ang
    }

    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val iva: ImageVirtualAccessor = input._2.image
      @tailrec
      def iterContour(iter: Int)(contour: Array[Coordinate], energy: Double): Array[Coordinate] = {
        println(s"Contour energy: $energy")
        def pointEnergy(i: Int, p: Coordinate): Double = {
          val distCoff = msg.parameters._1
          val convCoff = msg.parameters._2
          val imgCoff = msg.parameters._3
          val left = contour(math.abs((i - 1) % contour.length))
          val right = contour((i + 1) % contour.length)
          val Edist = dist(left, p) + dist(p, right)
          val Ecurv = math.abs(angle(left, p, right))
          val Eimg = iva.nGet(contour(i)._1)(contour(i)._2)
          (distCoff * Edist) + (convCoff * Ecurv) + (imgCoff * Eimg)
        }

        def iterPoint(i: Int)(p: Coordinate, e: Double): (Coordinate, Double) = {
          val cords = (for(x <- 0 until iva.n; y <- 0 until iva.m) yield (x, y)).filter{
            p => {
              !contour.contains(p) && (0 <= p._1 && p._1 <= iva.n) && (0 <= p._2 && p._2 <= iva.m)
            }
          }
          val possible = cords.map((c: Coordinate) => (c, pointEnergy(i, c))).filter(_._2 < e).sortBy(_._2).toList
          Try(possible.head).getOrElse((p, e))
        }

        val pointEnergies = for (i <- 0 until contour.size) yield {
          iterPoint(i)(contour(i), pointEnergy(i, contour(i)))
        }
        val E: Double = pointEnergies.map(_._2).foldLeft(0d)(_ + _)
        println(s"Next enegry: $E")
        if (E < energy)
          iterContour(iter + 1)(pointEnergies.map(_._1).toArray, E)
        else
          contour
      }

      def contour2Image(contour: Array[Coordinate]): Image = {
        var img: ImageVirtualAccessor = Image((iva.m, iva.n), Vector.fill(iva.m, iva.n)(BLACK).flatten.toArray)
        for((m, n) <- contour) {
          img = img.set(n)(m)(WHITE)
        }
        Image(input._2.image.dimensions, img.pix)
      }
      PipelineMsg(input._1, input._3, contour2Image(iterContour(0)(input._1, Int.MaxValue)), task, msg.parameters)
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
  case class Position (x: Int, y: Int)
  case class Velocity (u: Double, v: Double)
  case class Trace(image: Image, coords: Option[Coordinate])
  case class Image(dimensions: (Int, Int), pixels: Array[Int])
  object Image {
    implicit def Image2Trace(img: Image): Trace = Trace(img, None)
    implicit def SKImage2Image(img: SKImage): Image = Image((img.width, img.height), img.pixels)
    implicit def Image2SKImage(img: Image): SKImage = SKImage(img.dimensions._1, img.dimensions._2, img.pixels)
    implicit def Image2ImageVirtualAccessor(img: Image): ImageVirtualAccessor = new ImageVirtualAccessor(img)
  }
   case class PipelineMsg(input: Option[(Array[Coordinate], Image, Array[Coordinate])], task: Option[Task], parameters: (Double, Double, Double))
  object PipelineMsg {
    def apply(trace: Array[Coordinate], centroids: Array[Coordinate], image: Image, task: Task, params: (Double, Double, Double)): PipelineMsg = {
      PipelineMsg(Some(trace, image, centroids), Some(task), params)
    }
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
  lazy val normalized = {
    val xord = pix.map(pix => (pix ^ 0xff000000).toDouble)
    val min: Double = xord.min
    val max: Double = xord.max
    val n = xord.map(p => (p - min) / (max - min))
    n
  }
  def get(Row: Int)(Col: Int): Int = {
    pix(Row * n + Col)
  }
  def nGet(row: Int)(col: Int): Double = {
    normalized(row * n + col)
  }

  def set(Row: Int)(Col: Int)(Pix: Int): ImageVirtualAccessor = {
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

