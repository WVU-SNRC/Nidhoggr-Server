package com.nidhoggr
import com.nidhoggr.NidhoggrPipeline.{PipelineMsg, Task}
import com.nidhoggr.NidhoggrPipeline.Image
import com.nidhoggr.NidhoggrPipeline.Image._
import java.io.File
import java.io.FileInputStream
import com.sksamuel.scrimage.io.TiffReader
import com.sksamuel.scrimage.{Image => SKImage}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object GenTestData extends App {
  def img2Contour(img: Image) = {
    img.pixels.zipWithIndex.filter(_._1 == -1).map{ case (value: Int, index: Int) =>
      val col = index % img.dimensions._1
      val row = index / img.dimensions._1
      (col, row)
    }
  }
  def runWithParams(distCoff: Double, convCoff: Double, imgCoff: Double) = {
    NidhoggrPipeline.runPipe((Some(NidhoggrPipeline()), PipelineMsg(contour, Array(c1, (396, 96)), tif, Task("", ""), (distCoff, convCoff, imgCoff))), 0)
  }
  def dist(p: (Int, Int), q: (Int, Int)) = {
    math.sqrt(math.pow(p._1 - q._1, 2) + math.pow(p._2 - q._2, 2))
  }

  def distance(genCon: Array[(Int, Int)], trueCon: Array[(Int, Int)]) = {
    genCon.map(p => trueCon.map(dist(p, _)).sorted.head).foldLeft(0d)(_ + _)
  }

  val contour = Array((386, 81), (395, 82), (402, 84), (408, 85), (411, 90), (411, 96), (408, 105), (398, 109), (388, 108), (378, 106), (373, 102), (371, 96), (371, 89), (375, 85), (381, 82))
  val trueCon = img2Contour(TiffReader.read(new FileInputStream(new File("""C:\Users\Spirou Group\Desktop\tifNidhogTest\trace.02.tif"""))))
  val c1 = (contour.map(_._1).foldLeft(0)(_ + _) / contour.length, contour.map(_._2).foldLeft(0)(_ + _) / contour.length)
  val tif = SKImage(new File("""C:\Users\Spirou Group\Desktop\tifNidhogTest\pngs\t.001.png"""))
  val pDist = mutable.Map[(Double, Double, Double), Double]()

  val params = for(distCoff <- -1d to(0, 0.5); convCoff <- 0d to(1, 0.5); imgCoff <- -0.1 to(-1, -0.5)) yield {
    val f = Future{
      val res = runWithParams(distCoff, convCoff, imgCoff)
      val contour = img2Contour(res.input.get._2)
      val d = distance(contour, trueCon)
      pDist((distCoff, convCoff, imgCoff)) = d
      println(s"${(distCoff, convCoff, imgCoff)}: $d")
      d
    }
    f.onFailure{ case e => e.printStackTrace() }
    f
  }
  val done = Future.sequence(params)
  Await.result(done, Duration.Inf)
  val best = pDist.toList.sortBy(_._2).head
  println(s"Done, best result: $best")
}
