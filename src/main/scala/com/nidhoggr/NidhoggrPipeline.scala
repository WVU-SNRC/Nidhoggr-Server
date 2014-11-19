package com.nidhoggr

import java.io.File

import com.nidhoggr.NidhoggrPipeline._
import scala.annotation.tailrec
import scala.language.implicitConversions
import com.sksamuel.scrimage.filter.{InvertFilter, GaussianBlurFilter, EdgeFilter}
import com.sksamuel.scrimage.{Image => SKImage}

case class AccuracyBelowThresholdException(task: Task) extends RuntimeException


object NidhoggrPipeline {
  val BLACK = -16777216
  val WHITE = -1
  import NidhoggrPipeline.Image._

  def apply():NidhoggrPipeline = {
    new NidhoggrPipeline(List(normalize, centralize, gaussianBlur, edgeDetection, axonOptimization))
  }

  @tailrec
  def runPipe(pipe: PipelineResult, i: Int): PipelineMsg = pipe match {
    case (Some(pipeline), msg) =>
      for(input <- msg.input) {
        val output: SKImage = input._2.image
        output.write(new File(s"pipe$i.png"))
      }
      runPipe(pipeline(msg), i + 1)
    case (None, msg) =>
      for(input <- msg.input) {
        val output: SKImage = input._2.image
        output.write(new File(s"output$i.png"))
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
      PipelineMsg(input._1.map(c => (c._1 - xoff, c._2 - yoff)), input._3.tail, input._2, task)
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
    PipelineMsg(msg.input.map((input) => (input._1, normalizer(input._2.image), input._3)), msg.task)
  }

  def edgeDetection(msg: PipelineMsg): PipelineMsg = {
    /*val sobelX = Array(Array(-1,0,1),Array(-2,0,2),Array(-1,0,1))
    val sobelY = Array(Array(1,2,1),Array(0,0,0),Array(-1,-2,-1))
    def convulution(img: Image):Image ={
      def iter(img:ImageVirtualAccessor):Image = {
        val xIndices = for (x<- 1 until img.n-1) yield x
        val yIndices = for (y<- 1 until img.m-1) yield y
        val indices = for{p<-xIndices;q<-yIndices} yield(p,q)
        Image((img.m-2,img.n-2),indices.map{case(a,b)=> {
          val pixelX = (sobelX(0)(0)*img.get(a-1)(b-1))+(sobelX(0)(1)*img.get(a)(b-1))+
            (sobelX(1)(0)*img.get(a-1)(b))+(sobelX(1)(1)*img.get(a)(b))+
            (sobelX(2)(0)*img.get(a-1)(b+1))+(sobelX(2)(1)*img.get(a)(b+1))
          val pixelY = (sobelY(0)(0)*img.get(a-1)(b-1))+(sobelY(0)(1)*img.get(a)(b-1))+
            (sobelY(1)(0)*img.get(a-1)(b))+(sobelY(1)(1)*img.get(a)(b))+
            (sobelY(2)(0)*img.get(a-1)(b+1))+(sobelY(2)(1)*img.get(a)(b+1))
          Math.ceil(Math.sqrt(Math.pow(pixelX,2)+Math.pow(pixelY,2))).toInt
        }}.toArray)
      }
      iter(new ImageVirtualAccessor(img))
    }
    PipelineMsg(msg.input.get._1,convulution(msg.input.get._2.image),msg.task.get)*/
    val res = for (
      input <- msg.input;
      task <- msg.task
    ) yield {
      val image = input._2
      val trace = input._1
      val raster = image.image.filter(EdgeFilter)
      PipelineMsg(trace, input._3, raster, task)
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
      PipelineMsg(trace, input._3, raster, task)
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
      PipelineMsg(trace, input._3, raster, task)
    }
    res.getOrElse(msg)
  }

  def axonOptimization(msg: PipelineMsg): PipelineMsg = {
    def dist(p: Coordinate, q: Coordinate) = {
      math.sqrt(math.pow(p._1 - q._1, 2) + math.pow(p._2 - q._2, 2))
    }
    def angle(left: Coordinate, middle: Coordinate, right: Coordinate) = {
      val ang = math.toDegrees(math.acos((math.pow(dist(middle, left), 2) + math.pow(dist(middle, right), 2) - math.pow(dist(left, right), 2))
        / (2 * dist(middle, left) * dist(middle, right))))
      println(s"suck it will: $ang for point: $left $middle $right")
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
      @tailrec
      def iterContour(iter: Int)(contour: Array[Coordinate], energy: Double): Array[Coordinate] = {
        println(s"Contour energy: $energy")
        def pointEnergy(i: Int, p: Coordinate): Double = {
          val left = contour(math.abs((i - 1) % contour.length))
          val right = contour((i + 1) % contour.length)
          (1 * (dist(left, p) + dist(p, right))) + (-20 * math.abs(angle(left, p, right))) + (-10 * input._2.image.nGet(contour(i)._1)(contour(i)._2))
        }

        @tailrec
        def iterPoint(i: Int)(p: Coordinate, e: Double): (Coordinate, Double) = {
          val cords = for (v <- 1 until 2) yield {
            List(
              (p._1 + v, p._2 + v),
              (p._1 + v, p._2),
              (p._1 + v, p._2 - v),
              (p._1, p._2 + v),
              (p._1, p._2 - v),
              (p._1 - v, p._2 + v),
              (p._1 - v, p._2),
              (p._1 - v, p._2 - v)
            )
          }.filter(!contour.contains(_))
          val possible = cords.flatten.map((c: Coordinate) => (c, pointEnergy(i, c))).filter(_._2 < e).sortBy(_._2).toList
          possible match {
            case n :: ns => iterPoint(i)(n._1, n._2)
            case Nil => (p, e)
          }
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
        var img: ImageVirtualAccessor = Image((input._2.image.m, input._2.image.n), Vector.fill(input._2.image.m, input._2.image.n)(BLACK).flatten.toArray)
        for((m, n) <- contour) {
          img = img.set(n)(m)(WHITE)
        }
        Image((input._2.image.m, input._2.image.n), img.pix)
      }
      PipelineMsg(input._1, input._3, contour2Image(iterContour(0)(input._1, Int.MaxValue)), task)
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
   case class PipelineMsg(input: Option[(Array[Coordinate], Image, Array[Coordinate])], task: Option[Task])
  object PipelineMsg {
    def apply(trace: Array[Coordinate], centroids: Array[Coordinate], image: Image, task: Task): PipelineMsg = {
      PipelineMsg(Some(trace, image, centroids), Some(task))
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
    val min: Double = data.pixels.min
    val max: Double = data.pixels.max
    val n = pix.map(p => (p.toDouble - min) / (max - min))
    n
  }
  def get(Row: Int)(Col: Int): Int = {
    pix.slice(Row*n,Row*n+n)(Col)
  }
  def nGet(row: Int)(col: Int): Double = {
    normalized.slice(row*n,row*n+n)(col)
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

