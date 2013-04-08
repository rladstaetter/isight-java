package net.ladstatt.apps.isight

import org.opencv.highgui.VideoCapture
import java.io.File
import org.opencv.core.Mat
import org.opencv.highgui.Highgui
import scala.collection.JavaConversions._
import org.opencv.core.CvType
import java.util.Date
import java.util.UUID

object VideoCaptureWithOpenCV {

  def main(args: Array[String]) {
    System.load(new File("/opt/local/share/OpenCV/java/libopencv_java244.dylib").getAbsolutePath())

    val videocapture = new VideoCapture(0)
    assert(videocapture.isOpened())
    while (videocapture.grab) {
      val image = new Mat()
      while (videocapture.read(image) == false) { println("waiting for successful grab") }
      val fn = "image_%s.png".format(UUID.randomUUID.toString())
      Highgui.imwrite(fn, image)
      println("grabbing photo ...")
    }
    println("now you have many photos of yourself :)")
  }
  
}