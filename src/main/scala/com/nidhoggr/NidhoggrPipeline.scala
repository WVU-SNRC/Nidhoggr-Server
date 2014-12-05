package com.nidhoggr

import java.io.File

import com.nidhoggr.NidhoggrPipeline._
import scala.annotation.tailrec
import scala.language.implicitConversions
import com.sksamuel.scrimage.filter._
import com.sksamuel.scrimage.{Image => SKImage}

import scala.util.Try



object NidhoggrPipeline {
  val BLACK = -16777216
  val WHITE = -1
  val distCoff = -7d
  val convCoff = 0.7
  val imgCoff = -1d
  import NidhoggrPipeline.Image._

  def apply(): NidhoggrPipeline = {
    new NidhoggrPipeline(List(centralize, gaussianBlur, edgeDetection, threashhold, axonOptimization))
  }

  @tailrec
  def runPipe(pipe: PipelineResult, i: Int): PipelineMsg = pipe match {
    case (Some(pipeline), msg) =>
      for(input <- msg.input) {
        val distCoff = msg.parameters._1
        val convCoff = msg.parameters._2
        val imgCoff = msg.parameters._3
        val output: SKImage = input._2.image
        output.write(new File(s"pipe$i.png"))
      }
      runPipe(pipeline(msg), i + 1)
    case (None, msg) =>
      for(input <- msg.input) {
        val distCoff = msg.parameters._1
        val convCoff = msg.parameters._2
        val imgCoff = msg.parameters._3
        val output: SKImage = input._2.image
        output.write(new File(s"output $distCoff $convCoff $imgCoff.png"))
      }
      msg
  }

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
    def normalizer(img: Image):Image = {
      val nW = WHITE ^ 0xff000000
      val iva: ImageVirtualAccessor = img
      val min = iva.normalized.min
      val max = iva.normalized.max
      Image(img.dimensions, iva.normalized.map(_ * nW).map(_ | 0xff000000))
    }
    PipelineMsg(msg.input.map((input) => (input._1, normalizer(input._2.image), input._3)), msg.task, msg.parameters)
  }

  def threashhold(msg: PipelineMsg): PipelineMsg = {
    def threashit(img: Image): Image = {
      val black = 0xff000000
      val notSoBlack = 0xff333333
      Image(img.dimensions, img.pixels.map{ p =>
        if(p < notSoBlack){
          black
        } else {
          p
        }
      })
    }
    PipelineMsg(msg.input.map((input) => (input._1, threashit(input._2.image), input._3)), msg.task, msg.parameters)
  }

  def edgeDetection(msg: PipelineMsg): PipelineMsg = {
    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val image = input._2
      val trace = input._1
      val raster = image.image.filter(EdgeFilter)
      val brighter = raster.filter(BrightnessFilter(3))
      PipelineMsg(trace, input._3, brighter, task, msg.parameters)
    }
    res.get
  }


  def gaussianBlur(msg: PipelineMsg): PipelineMsg = {
    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val image = input._2
      val trace = input._1
      val raster = image.image.filter(GaussianBlurFilter(10))
      PipelineMsg(trace, input._3, raster, task, msg.parameters)
    }
    res.get
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
      val (a, b) = (right._2 - left._2, right._1 / left._1)
      val c = (-left._1 * a) + (-left._2 + b)
      val x = ((b * ((b * right._1) - (a * right._2))) - (a * c)) / (math.pow(a, 2) + math.pow(b, 2))
      val y = ((a * ((-b * right._1) + (a * right._2))) - (b * c)) / (math.pow(a, 2) + math.pow(b, 2))
      dist(middle, (x.toInt, y.toInt))
    }



    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val iva: ImageVirtualAccessor = input._2.image
      @tailrec
      def iterContour(iter: Int)(contour: Array[Coordinate], energy: Double, sign: Int): Array[Coordinate] = {
        val centroid = (contour.map(_._1).foldLeft(0)(_ + _) / contour.length, contour.map(_._2).foldLeft(0)(_ + _) / contour.length)
        println(s"centroid: $centroid")
        val im = contour2Image(contour :+ centroid)
        im.write(new File(s"c.iter.$iter.png"))
        contour.map(print)
        println()
        def pointEnergy(i: Int, p: Coordinate): Double = {
          val distCoff = msg.parameters._1
          val convCoff = msg.parameters._2
          val imgCoff = msg.parameters._3
          val left = contour(math.abs((i - 1) % contour.length))
          val right = contour((i + 1) % contour.length)
          val Edist = dist(p, centroid)
          val Ecurv = math.abs(angle(left, p, right))
          val Eimg = iva.nGet(p._1)(p._2)
          ((sign * distCoff) * Edist) + (convCoff * Ecurv) + (imgCoff * Eimg)
        }

        def iterPoint(i: Int)(p: Coordinate, e: Double): (Coordinate, Double) = {
          val cords = (for(x <- (p._1 - 5) until (p._1 + 5); y <- (p._2 - 5) until (p._2 + 5)) yield (x, y)).filter{
            pix => {
              !contour.contains(pix)
            }
          }
          val possible = cords.map((c: Coordinate) => (c, pointEnergy(i, c))).filter(_._2 < e).sortBy(_._2).toList
          Try(possible.head).getOrElse((p, e))
        }

        val pointEnergies = for (i <- 0 until contour.size) yield {
          iterPoint(i)(contour(i), pointEnergy(i, contour(i)))
        }
        val E: Double = pointEnergies.map(_._2).foldLeft(0d)(_ + _)
        if (E < energy)
          iterContour(iter + 1)(pointEnergies.map(_._1).toArray, E, sign)
        else
          contour
      }

      def contour2Image(contour: Array[Coordinate]): Image = {
        var img: ImageVirtualAccessor = Image((iva.m, iva.n), Vector.fill(iva.m, iva.n)(BLACK).flatten.toArray)
        for((m, n) <- contour) {
          img = img.set(m)(n)(WHITE)
        }
        Image(input._2.image.dimensions, img.pix)
      }
      PipelineMsg(input._1, input._3, contour2Image(iterContour(0)(input._1, Int.MaxValue, -1)), task, msg.parameters)
    }
    res.get
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
    pix.map(pix => pix & ~0xffffff00)
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

