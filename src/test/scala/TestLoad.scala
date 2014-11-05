import java.io.{FileInputStream, File}

import scala.sys.process._

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.filter.EdgeFilter
import com.sksamuel.scrimage.io.TiffReader
import org.scalatest._


class TestLoad extends FlatSpec with Matchers {
  val test16 = new File("src/test/resources/test16.tif")
  val test = new File("src/test/resources/test.tif")
  val testEdges = new File("src/test/resources/testEdges.png")
  var tif: Image = null
  var edges: Image = null

  "ImageMagick" should s"convert ${test16.getCanonicalPath} to 8 bit and save it as ${test.getCanonicalPath}" in {
    s"""convert "${test16.getCanonicalPath}" -normalize -depth 8 -colors 256 -type grayscale "${test.getCanonicalPath}"""".! should be (0)
  }

  "A TiffReader" should s"load an image, ${test.getCanonicalPath}" in {
    tif = TiffReader.read(new FileInputStream(test))
  }

  it should "perform edge detection on the image" in {
    edges = tif.filter(EdgeFilter)
  }

  "A TiffWriter" should "save the image as testEdges.tif" in {
    edges.write(testEdges)
  }
}
