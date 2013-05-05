package net.ladstatt.apps.isight

import java.io.ByteArrayInputStream
import java.io.File

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.JavaConversions.seqAsJavaList
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.opencv.core.Core
import org.opencv.core.Mat
import org.opencv.core.MatOfByte
import org.opencv.core.MatOfRect
import org.opencv.core.Point
import org.opencv.core.Range
import org.opencv.core.Scalar
import org.opencv.core.Size
import org.opencv.highgui.Highgui
import org.opencv.highgui.VideoCapture
import org.opencv.imgproc.Imgproc
import org.opencv.objdetect.CascadeClassifier

import javafx.application.Application
import javafx.beans.property.SimpleObjectProperty
import javafx.concurrent.Service
import javafx.concurrent.Task
import javafx.event.Event
import javafx.event.EventHandler
import javafx.geometry.Orientation
import javafx.scene.Scene
import javafx.scene.control.ComboBox
import javafx.scene.control.Label
import javafx.scene.control.ListCell
import javafx.scene.control.ListView
import javafx.scene.control.Slider
import javafx.scene.control.ToggleButton
import javafx.scene.image.Image
import javafx.scene.image.ImageView
import javafx.scene.layout.BorderPane
import javafx.scene.layout.HBox
import javafx.scene.text.Font
import javafx.stage.Stage
import javafx.util.Callback

import OpenCV._

/**
 * For a discussion of the concepts of this application see http://ladstatt.blogspot.com/
 */
trait Utils {

  lazy val runOnMac =
    {
      System.getProperty("os.name").toLowerCase match {
        case"mac os x" => true
        case _ => false
      }
    }

  def either[L, R, X](e: Either[L, R])(left: => L => X, right: => R => X): X = {
    e match {
      case Left(l) => left(l)
      case Right(r) => right(r)
    }
  }
  
  def attempt[U,X](e: Try[U])(onFailure: => Throwable => X, onSuccess: => U => X): X = {
    e match {
    	  case Failure(e) => onFailure(e)
      case Success(value) => onSuccess(value)
    }
  }
  
  
}

object OpenCV {

  case class ImgprocColor(val id: String, value: Int) {
    override def toString = id
  }

  val colorConstants =
    Map(
     "ADAPTIVE_THRESH_GAUSSIAN_C" -> ImgprocColor("ADAPTIVE_THRESH_GAUSSIAN_C", Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C),
     "ADAPTIVE_THRESH_MEAN_C" -> ImgprocColor("ADAPTIVE_THRESH_MEAN_C", Imgproc.ADAPTIVE_THRESH_MEAN_C),
     "BORDER_CONSTANT" -> ImgprocColor("BORDER_CONSTANT", Imgproc.BORDER_CONSTANT),
     "BORDER_DEFAULT" -> ImgprocColor("BORDER_DEFAULT", Imgproc.BORDER_DEFAULT),
     "BORDER_REFLECT" -> ImgprocColor("BORDER_REFLECT", Imgproc.BORDER_REFLECT),
     "BORDER_REFLECT101" -> ImgprocColor("BORDER_REFLECT101", Imgproc.BORDER_REFLECT101),
     "BORDER_REFLECT_101" -> ImgprocColor("BORDER_REFLECT_101", Imgproc.BORDER_REFLECT_101),
     "BORDER_REPLICATE" -> ImgprocColor("BORDER_REPLICATE", Imgproc.BORDER_REPLICATE),
     "BORDER_TRANSPARENT" -> ImgprocColor("BORDER_TRANSPARENT", Imgproc.BORDER_TRANSPARENT),
     "BORDER_WRAP" -> ImgprocColor("BORDER_WRAP", Imgproc.BORDER_WRAP),
     "CHAIN_APPROX_NONE" -> ImgprocColor("CHAIN_APPROX_NONE", Imgproc.CHAIN_APPROX_NONE),
     "CHAIN_APPROX_SIMPLE" -> ImgprocColor("CHAIN_APPROX_SIMPLE", Imgproc.CHAIN_APPROX_SIMPLE),
     "CHAIN_APPROX_TC89_KCOS" -> ImgprocColor("CHAIN_APPROX_TC89_KCOS", Imgproc.CHAIN_APPROX_TC89_KCOS),
     "CHAIN_APPROX_TC89_L1" -> ImgprocColor("CHAIN_APPROX_TC89_L1", Imgproc.CHAIN_APPROX_TC89_L1),
     "COLOR_BGR2BGRA" -> ImgprocColor("COLOR_BGR2BGRA", Imgproc.COLOR_BGR2BGRA),
     "COLOR_BGR2GRAY" -> ImgprocColor("COLOR_BGR2GRAY", Imgproc.COLOR_BGR2GRAY),
     "COLOR_BGR2HLS" -> ImgprocColor("COLOR_BGR2HLS", Imgproc.COLOR_BGR2HLS),
     "COLOR_BGR2HLS_FULL" -> ImgprocColor("COLOR_BGR2HLS_FULL", Imgproc.COLOR_BGR2HLS_FULL),
     "COLOR_BGR2HSV" -> ImgprocColor("COLOR_BGR2HSV", Imgproc.COLOR_BGR2HSV),
     "COLOR_BGR2HSV_FULL" -> ImgprocColor("COLOR_BGR2HSV_FULL", Imgproc.COLOR_BGR2HSV_FULL),
     "COLOR_BGR2Lab" -> ImgprocColor("COLOR_BGR2Lab", Imgproc.COLOR_BGR2Lab),
     "COLOR_BGR2Luv" -> ImgprocColor("COLOR_BGR2Luv", Imgproc.COLOR_BGR2Luv),
     "COLOR_BGR2RGB" -> ImgprocColor("COLOR_BGR2RGB", Imgproc.COLOR_BGR2RGB),
     "COLOR_BGR2RGBA" -> ImgprocColor("COLOR_BGR2RGBA", Imgproc.COLOR_BGR2RGBA),
     "COLOR_BGR2XYZ" -> ImgprocColor("COLOR_BGR2XYZ", Imgproc.COLOR_BGR2XYZ),
     "COLOR_BGR2YCrCb" -> ImgprocColor("COLOR_BGR2YCrCb", Imgproc.COLOR_BGR2YCrCb),
     "COLOR_BGR2YUV" -> ImgprocColor("COLOR_BGR2YUV", Imgproc.COLOR_BGR2YUV),
     "COLOR_BGRA2BGR" -> ImgprocColor("COLOR_BGRA2BGR", Imgproc.COLOR_BGRA2BGR),
     "COLOR_BGRA2GRAY" -> ImgprocColor("COLOR_BGRA2GRAY", Imgproc.COLOR_BGRA2GRAY),
     "COLOR_BGRA2RGB" -> ImgprocColor("COLOR_BGRA2RGB", Imgproc.COLOR_BGRA2RGB),
     "COLOR_BGRA2RGBA" -> ImgprocColor("COLOR_BGRA2RGBA", Imgproc.COLOR_BGRA2RGBA),
     "COLOR_BayerBG2RGB_VNG" -> ImgprocColor("COLOR_BayerBG2RGB_VNG", Imgproc.COLOR_BayerBG2RGB_VNG),
     "COLOR_BayerGB2BGR" -> ImgprocColor("COLOR_BayerGB2BGR", Imgproc.COLOR_BayerGB2BGR),
     "COLOR_BayerGB2BGR_VNG" -> ImgprocColor("COLOR_BayerGB2BGR_VNG", Imgproc.COLOR_BayerGB2BGR_VNG),
     "COLOR_BayerGB2GRAY" -> ImgprocColor("COLOR_BayerGB2GRAY", Imgproc.COLOR_BayerGB2GRAY),
     "COLOR_BayerGB2RGB" -> ImgprocColor("COLOR_BayerGB2RGB", Imgproc.COLOR_BayerGB2RGB),
     "COLOR_BayerGB2RGB_VNG" -> ImgprocColor("COLOR_BayerGB2RGB_VNG", Imgproc.COLOR_BayerGB2RGB_VNG),
     "COLOR_BayerGR2BGR" -> ImgprocColor("COLOR_BayerGR2BGR", Imgproc.COLOR_BayerGR2BGR),
     "COLOR_BayerGR2BGR_VNG" -> ImgprocColor("COLOR_BayerGR2BGR_VNG", Imgproc.COLOR_BayerGR2BGR_VNG),
     "COLOR_BayerGR2GRAY" -> ImgprocColor("COLOR_BayerGR2GRAY", Imgproc.COLOR_BayerGR2GRAY),
     "COLOR_BayerGR2RGB" -> ImgprocColor("COLOR_BayerGR2RGB", Imgproc.COLOR_BayerGR2RGB),
     "COLOR_BayerGR2RGB_VNG" -> ImgprocColor("COLOR_BayerGR2RGB_VNG", Imgproc.COLOR_BayerGR2RGB_VNG),
     "COLOR_BayerRG2BGR" -> ImgprocColor("COLOR_BayerRG2BGR", Imgproc.COLOR_BayerRG2BGR),
     "COLOR_BayerRG2BGR_VNG" -> ImgprocColor("COLOR_BayerRG2BGR_VNG", Imgproc.COLOR_BayerRG2BGR_VNG),
     "COLOR_BayerRG2GRAY" -> ImgprocColor("COLOR_BayerRG2GRAY", Imgproc.COLOR_BayerRG2GRAY),
     "COLOR_BayerRG2RGB" -> ImgprocColor("COLOR_BayerRG2RGB", Imgproc.COLOR_BayerRG2RGB),
     "COLOR_BayerRG2RGB_VNG" -> ImgprocColor("COLOR_BayerRG2RGB_VNG", Imgproc.COLOR_BayerRG2RGB_VNG),
     "COLOR_COLORCVT_MAX" -> ImgprocColor("COLOR_COLORCVT_MAX", Imgproc.COLOR_COLORCVT_MAX),
     "COLOR_GRAY2BGR" -> ImgprocColor("COLOR_GRAY2BGR", Imgproc.COLOR_GRAY2BGR),
     "COLOR_GRAY2BGRA" -> ImgprocColor("COLOR_GRAY2BGRA", Imgproc.COLOR_GRAY2BGRA),
     "COLOR_GRAY2RGB" -> ImgprocColor("COLOR_GRAY2RGB", Imgproc.COLOR_GRAY2RGB),
     "COLOR_GRAY2RGBA" -> ImgprocColor("COLOR_GRAY2RGBA", Imgproc.COLOR_GRAY2RGBA),
     "COLOR_HLS2BGR" -> ImgprocColor("COLOR_HLS2BGR", Imgproc.COLOR_HLS2BGR),
     "COLOR_HLS2BGR_FULL" -> ImgprocColor("COLOR_HLS2BGR_FULL", Imgproc.COLOR_HLS2BGR_FULL),
     "COLOR_HLS2RGB" -> ImgprocColor("COLOR_HLS2RGB", Imgproc.COLOR_HLS2RGB),
     "COLOR_HLS2RGB_FULL" -> ImgprocColor("COLOR_HLS2RGB_FULL", Imgproc.COLOR_HLS2RGB_FULL),
     "COLOR_HSV2BGR" -> ImgprocColor("COLOR_HSV2BGR", Imgproc.COLOR_HSV2BGR),
     "COLOR_HSV2BGR_FULL" -> ImgprocColor("COLOR_HSV2BGR_FULL", Imgproc.COLOR_HSV2BGR_FULL),
     "COLOR_HSV2RGB" -> ImgprocColor("COLOR_HSV2RGB", Imgproc.COLOR_HSV2RGB),
     "COLOR_HSV2RGB_FULL" -> ImgprocColor("COLOR_HSV2RGB_FULL", Imgproc.COLOR_HSV2RGB_FULL),
     "COLOR_LBGR2Lab" -> ImgprocColor("COLOR_LBGR2Lab", Imgproc.COLOR_LBGR2Lab),
     "COLOR_LBGR2Luv" -> ImgprocColor("COLOR_LBGR2Luv", Imgproc.COLOR_LBGR2Luv),
     "COLOR_LRGB2Lab" -> ImgprocColor("COLOR_LRGB2Lab", Imgproc.COLOR_LRGB2Lab),
     "COLOR_LRGB2Luv" -> ImgprocColor("COLOR_LRGB2Luv", Imgproc.COLOR_LRGB2Luv),
     "COLOR_Lab2BGR" -> ImgprocColor("COLOR_Lab2BGR", Imgproc.COLOR_Lab2BGR),
     "COLOR_Lab2LBGR" -> ImgprocColor("COLOR_Lab2LBGR", Imgproc.COLOR_Lab2LBGR),
     "COLOR_Lab2LRGB" -> ImgprocColor("COLOR_Lab2LRGB", Imgproc.COLOR_Lab2LRGB),
     "COLOR_Lab2RGB" -> ImgprocColor("COLOR_Lab2RGB", Imgproc.COLOR_Lab2RGB),
     "COLOR_Luv2BGR" -> ImgprocColor("COLOR_Luv2BGR", Imgproc.COLOR_Luv2BGR),
     "COLOR_Luv2LBGR" -> ImgprocColor("COLOR_Luv2LBGR", Imgproc.COLOR_Luv2LBGR),
     "COLOR_Luv2LRGB" -> ImgprocColor("COLOR_Luv2LRGB", Imgproc.COLOR_Luv2LRGB),
     "COLOR_Luv2RGB" -> ImgprocColor("COLOR_Luv2RGB", Imgproc.COLOR_Luv2RGB),
     "COLOR_RGB2BGR" -> ImgprocColor("COLOR_RGB2BGR", Imgproc.COLOR_RGB2BGR),
     "COLOR_RGB2BGRA" -> ImgprocColor("COLOR_RGB2BGRA", Imgproc.COLOR_RGB2BGRA),
     "COLOR_RGB2GRAY" -> ImgprocColor("COLOR_RGB2GRAY", Imgproc.COLOR_RGB2GRAY),
     "COLOR_RGB2HLS" -> ImgprocColor("COLOR_RGB2HLS", Imgproc.COLOR_RGB2HLS),
     "COLOR_RGB2HLS_FULL" -> ImgprocColor("COLOR_RGB2HLS_FULL", Imgproc.COLOR_RGB2HLS_FULL),
     "COLOR_RGB2HSV" -> ImgprocColor("COLOR_RGB2HSV", Imgproc.COLOR_RGB2HSV),
     "COLOR_RGB2HSV_FULL" -> ImgprocColor("COLOR_RGB2HSV_FULL", Imgproc.COLOR_RGB2HSV_FULL),
     "COLOR_RGB2Lab" -> ImgprocColor("COLOR_RGB2Lab", Imgproc.COLOR_RGB2Lab),
     "COLOR_RGB2Luv" -> ImgprocColor("COLOR_RGB2Luv", Imgproc.COLOR_RGB2Luv),
     "COLOR_RGB2RGBA" -> ImgprocColor("COLOR_RGB2RGBA", Imgproc.COLOR_RGB2RGBA),
     "COLOR_RGB2XYZ" -> ImgprocColor("COLOR_RGB2XYZ", Imgproc.COLOR_RGB2XYZ),
     "COLOR_RGB2YCrCb" -> ImgprocColor("COLOR_RGB2YCrCb", Imgproc.COLOR_RGB2YCrCb),
     "COLOR_RGB2YUV" -> ImgprocColor("COLOR_RGB2YUV", Imgproc.COLOR_RGB2YUV),
     "COLOR_RGBA2BGR" -> ImgprocColor("COLOR_RGBA2BGR", Imgproc.COLOR_RGBA2BGR),
     "COLOR_RGBA2BGRA" -> ImgprocColor("COLOR_RGBA2BGRA", Imgproc.COLOR_RGBA2BGRA),
     "COLOR_RGBA2GRAY" -> ImgprocColor("COLOR_RGBA2GRAY", Imgproc.COLOR_RGBA2GRAY),
     "COLOR_RGBA2RGB" -> ImgprocColor("COLOR_RGBA2RGB", Imgproc.COLOR_RGBA2RGB),
     "COLOR_RGBA2mRGBA" -> ImgprocColor("COLOR_RGBA2mRGBA", Imgproc.COLOR_RGBA2mRGBA),
     "COLOR_XYZ2BGR" -> ImgprocColor("COLOR_XYZ2BGR", Imgproc.COLOR_XYZ2BGR),
     "COLOR_XYZ2RGB" -> ImgprocColor("COLOR_XYZ2RGB", Imgproc.COLOR_XYZ2RGB),
     "COLOR_YCrCb2BGR" -> ImgprocColor("COLOR_YCrCb2BGR", Imgproc.COLOR_YCrCb2BGR),
     "COLOR_YCrCb2RGB" -> ImgprocColor("COLOR_YCrCb2RGB", Imgproc.COLOR_YCrCb2RGB),
     "COLOR_YUV2BGR" -> ImgprocColor("COLOR_YUV2BGR", Imgproc.COLOR_YUV2BGR),
     "COLOR_YUV2BGRA_I420" -> ImgprocColor("COLOR_YUV2BGRA_I420", Imgproc.COLOR_YUV2BGRA_I420),
     "COLOR_YUV2BGRA_IYUV" -> ImgprocColor("COLOR_YUV2BGRA_IYUV", Imgproc.COLOR_YUV2BGRA_IYUV),
     "COLOR_YUV2BGRA_NV12" -> ImgprocColor("COLOR_YUV2BGRA_NV12", Imgproc.COLOR_YUV2BGRA_NV12),
     "COLOR_YUV2BGRA_NV21" -> ImgprocColor("COLOR_YUV2BGRA_NV21", Imgproc.COLOR_YUV2BGRA_NV21),
     "COLOR_YUV2BGRA_UYNV" -> ImgprocColor("COLOR_YUV2BGRA_UYNV", Imgproc.COLOR_YUV2BGRA_UYNV),
     "COLOR_YUV2BGRA_UYVY" -> ImgprocColor("COLOR_YUV2BGRA_UYVY", Imgproc.COLOR_YUV2BGRA_UYVY),
     "COLOR_YUV2BGRA_Y422" -> ImgprocColor("COLOR_YUV2BGRA_Y422", Imgproc.COLOR_YUV2BGRA_Y422),
     "COLOR_YUV2BGRA_YUNV" -> ImgprocColor("COLOR_YUV2BGRA_YUNV", Imgproc.COLOR_YUV2BGRA_YUNV),
     "COLOR_YUV2BGRA_YUY2" -> ImgprocColor("COLOR_YUV2BGRA_YUY2", Imgproc.COLOR_YUV2BGRA_YUY2),
     "COLOR_YUV2BGRA_YUYV" -> ImgprocColor("COLOR_YUV2BGRA_YUYV", Imgproc.COLOR_YUV2BGRA_YUYV),
     "COLOR_YUV2BGRA_YV12" -> ImgprocColor("COLOR_YUV2BGRA_YV12", Imgproc.COLOR_YUV2BGRA_YV12),
     "COLOR_YUV2BGRA_YVYU" -> ImgprocColor("COLOR_YUV2BGRA_YVYU", Imgproc.COLOR_YUV2BGRA_YVYU),
     "COLOR_YUV2BGR_I420" -> ImgprocColor("COLOR_YUV2BGR_I420", Imgproc.COLOR_YUV2BGR_I420),
     "COLOR_YUV2BGR_IYUV" -> ImgprocColor("COLOR_YUV2BGR_IYUV", Imgproc.COLOR_YUV2BGR_IYUV),
     "COLOR_YUV2BGR_NV12" -> ImgprocColor("COLOR_YUV2BGR_NV12", Imgproc.COLOR_YUV2BGR_NV12),
     "COLOR_YUV2BGR_NV21" -> ImgprocColor("COLOR_YUV2BGR_NV21", Imgproc.COLOR_YUV2BGR_NV21),
     "COLOR_YUV2BGR_UYNV" -> ImgprocColor("COLOR_YUV2BGR_UYNV", Imgproc.COLOR_YUV2BGR_UYNV),
     "COLOR_YUV2BGR_UYVY" -> ImgprocColor("COLOR_YUV2BGR_UYVY", Imgproc.COLOR_YUV2BGR_UYVY),
     "COLOR_YUV2BGR_Y422" -> ImgprocColor("COLOR_YUV2BGR_Y422", Imgproc.COLOR_YUV2BGR_Y422),
     "COLOR_YUV2BGR_YUNV" -> ImgprocColor("COLOR_YUV2BGR_YUNV", Imgproc.COLOR_YUV2BGR_YUNV),
     "COLOR_YUV2BGR_YUY2" -> ImgprocColor("COLOR_YUV2BGR_YUY2", Imgproc.COLOR_YUV2BGR_YUY2),
     "COLOR_YUV2BGR_YUYV" -> ImgprocColor("COLOR_YUV2BGR_YUYV", Imgproc.COLOR_YUV2BGR_YUYV),
     "COLOR_YUV2BGR_YV12" -> ImgprocColor("COLOR_YUV2BGR_YV12", Imgproc.COLOR_YUV2BGR_YV12),
     "COLOR_YUV2BGR_YVYU" -> ImgprocColor("COLOR_YUV2BGR_YVYU", Imgproc.COLOR_YUV2BGR_YVYU),
     "COLOR_YUV2GRAY_420" -> ImgprocColor("COLOR_YUV2GRAY_420", Imgproc.COLOR_YUV2GRAY_420),
     "COLOR_YUV2GRAY_I420" -> ImgprocColor("COLOR_YUV2GRAY_I420", Imgproc.COLOR_YUV2GRAY_I420),
     "COLOR_YUV2GRAY_IYUV" -> ImgprocColor("COLOR_YUV2GRAY_IYUV", Imgproc.COLOR_YUV2GRAY_IYUV),
     "COLOR_YUV2GRAY_NV12" -> ImgprocColor("COLOR_YUV2GRAY_NV12", Imgproc.COLOR_YUV2GRAY_NV12),
     "COLOR_YUV2GRAY_NV21" -> ImgprocColor("COLOR_YUV2GRAY_NV21", Imgproc.COLOR_YUV2GRAY_NV21),
     "COLOR_YUV2GRAY_UYNV" -> ImgprocColor("COLOR_YUV2GRAY_UYNV", Imgproc.COLOR_YUV2GRAY_UYNV),
     "COLOR_YUV2GRAY_UYVY" -> ImgprocColor("COLOR_YUV2GRAY_UYVY", Imgproc.COLOR_YUV2GRAY_UYVY),
     "COLOR_YUV2GRAY_Y422" -> ImgprocColor("COLOR_YUV2GRAY_Y422", Imgproc.COLOR_YUV2GRAY_Y422),
     "COLOR_YUV2GRAY_YUNV" -> ImgprocColor("COLOR_YUV2GRAY_YUNV", Imgproc.COLOR_YUV2GRAY_YUNV),
     "COLOR_YUV2GRAY_YUY2" -> ImgprocColor("COLOR_YUV2GRAY_YUY2", Imgproc.COLOR_YUV2GRAY_YUY2),
     "COLOR_YUV2GRAY_YUYV" -> ImgprocColor("COLOR_YUV2GRAY_YUYV", Imgproc.COLOR_YUV2GRAY_YUYV),
     "COLOR_YUV2GRAY_YV12" -> ImgprocColor("COLOR_YUV2GRAY_YV12", Imgproc.COLOR_YUV2GRAY_YV12),
     "COLOR_YUV2GRAY_YVYU" -> ImgprocColor("COLOR_YUV2GRAY_YVYU", Imgproc.COLOR_YUV2GRAY_YVYU),
     "COLOR_YUV2RGB" -> ImgprocColor("COLOR_YUV2RGB", Imgproc.COLOR_YUV2RGB),
     "COLOR_YUV2RGBA_I420" -> ImgprocColor("COLOR_YUV2RGBA_I420", Imgproc.COLOR_YUV2RGBA_I420),
     "COLOR_YUV2RGBA_IYUV" -> ImgprocColor("COLOR_YUV2RGBA_IYUV", Imgproc.COLOR_YUV2RGBA_IYUV),
     "COLOR_YUV2RGBA_NV12" -> ImgprocColor("COLOR_YUV2RGBA_NV12", Imgproc.COLOR_YUV2RGBA_NV12),
     "COLOR_YUV2RGBA_NV21" -> ImgprocColor("COLOR_YUV2RGBA_NV21", Imgproc.COLOR_YUV2RGBA_NV21),
     "COLOR_YUV2RGBA_UYNV" -> ImgprocColor("COLOR_YUV2RGBA_UYNV", Imgproc.COLOR_YUV2RGBA_UYNV),
     "COLOR_YUV2RGBA_UYVY" -> ImgprocColor("COLOR_YUV2RGBA_UYVY", Imgproc.COLOR_YUV2RGBA_UYVY),
     "COLOR_YUV2RGBA_Y422" -> ImgprocColor("COLOR_YUV2RGBA_Y422", Imgproc.COLOR_YUV2RGBA_Y422),
     "COLOR_YUV2RGBA_YUNV" -> ImgprocColor("COLOR_YUV2RGBA_YUNV", Imgproc.COLOR_YUV2RGBA_YUNV),
     "COLOR_YUV2RGBA_YUY2" -> ImgprocColor("COLOR_YUV2RGBA_YUY2", Imgproc.COLOR_YUV2RGBA_YUY2),
     "COLOR_YUV2RGBA_YUYV" -> ImgprocColor("COLOR_YUV2RGBA_YUYV", Imgproc.COLOR_YUV2RGBA_YUYV),
     "COLOR_YUV2RGBA_YV12" -> ImgprocColor("COLOR_YUV2RGBA_YV12", Imgproc.COLOR_YUV2RGBA_YV12),
     "COLOR_YUV2RGBA_YVYU" -> ImgprocColor("COLOR_YUV2RGBA_YVYU", Imgproc.COLOR_YUV2RGBA_YVYU),
     "COLOR_YUV2RGB_I420" -> ImgprocColor("COLOR_YUV2RGB_I420", Imgproc.COLOR_YUV2RGB_I420),
     "COLOR_YUV2RGB_IYUV" -> ImgprocColor("COLOR_YUV2RGB_IYUV", Imgproc.COLOR_YUV2RGB_IYUV),
     "COLOR_YUV2RGB_NV12" -> ImgprocColor("COLOR_YUV2RGB_NV12", Imgproc.COLOR_YUV2RGB_NV12),
     "COLOR_YUV2RGB_NV21" -> ImgprocColor("COLOR_YUV2RGB_NV21", Imgproc.COLOR_YUV2RGB_NV21),
     "COLOR_YUV2RGB_UYNV" -> ImgprocColor("COLOR_YUV2RGB_UYNV", Imgproc.COLOR_YUV2RGB_UYNV),
     "COLOR_YUV2RGB_UYVY" -> ImgprocColor("COLOR_YUV2RGB_UYVY", Imgproc.COLOR_YUV2RGB_UYVY),
     "COLOR_YUV2RGB_Y422" -> ImgprocColor("COLOR_YUV2RGB_Y422", Imgproc.COLOR_YUV2RGB_Y422),
     "COLOR_YUV2RGB_YUNV" -> ImgprocColor("COLOR_YUV2RGB_YUNV", Imgproc.COLOR_YUV2RGB_YUNV),
     "COLOR_YUV2RGB_YUY2" -> ImgprocColor("COLOR_YUV2RGB_YUY2", Imgproc.COLOR_YUV2RGB_YUY2),
     "COLOR_YUV2RGB_YUYV" -> ImgprocColor("COLOR_YUV2RGB_YUYV", Imgproc.COLOR_YUV2RGB_YUYV),
     "COLOR_YUV2RGB_YV12" -> ImgprocColor("COLOR_YUV2RGB_YV12", Imgproc.COLOR_YUV2RGB_YV12),
     "COLOR_YUV2RGB_YVYU" -> ImgprocColor("COLOR_YUV2RGB_YVYU", Imgproc.COLOR_YUV2RGB_YVYU),
     "COLOR_YUV420p2BGR" -> ImgprocColor("COLOR_YUV420p2BGR", Imgproc.COLOR_YUV420p2BGR),
     "COLOR_YUV420p2BGRA" -> ImgprocColor("COLOR_YUV420p2BGRA", Imgproc.COLOR_YUV420p2BGRA),
     "COLOR_YUV420p2GRAY" -> ImgprocColor("COLOR_YUV420p2GRAY", Imgproc.COLOR_YUV420p2GRAY),
     "COLOR_YUV420p2RGB" -> ImgprocColor("COLOR_YUV420p2RGB", Imgproc.COLOR_YUV420p2RGB),
     "COLOR_YUV420p2RGBA" -> ImgprocColor("COLOR_YUV420p2RGBA", Imgproc.COLOR_YUV420p2RGBA),
     "COLOR_YUV420sp2BGR" -> ImgprocColor("COLOR_YUV420sp2BGR", Imgproc.COLOR_YUV420sp2BGR),
     "COLOR_YUV420sp2BGRA" -> ImgprocColor("COLOR_YUV420sp2BGRA", Imgproc.COLOR_YUV420sp2BGRA),
     "COLOR_YUV420sp2GRAY" -> ImgprocColor("COLOR_YUV420sp2GRAY", Imgproc.COLOR_YUV420sp2GRAY),
     "COLOR_YUV420sp2RGB" -> ImgprocColor("COLOR_YUV420sp2RGB", Imgproc.COLOR_YUV420sp2RGB),
     "COLOR_YUV420sp2RGBA" -> ImgprocColor("COLOR_YUV420sp2RGBA", Imgproc.COLOR_YUV420sp2RGBA),
     "COLOR_mRGBA2RGBA" -> ImgprocColor("COLOR_mRGBA2RGBA", Imgproc.COLOR_mRGBA2RGBA),
     "CV_BILATERAL" -> ImgprocColor("CV_BILATERAL", Imgproc.CV_BILATERAL),
     "CV_BLUR" -> ImgprocColor("CV_BLUR", Imgproc.CV_BLUR),
     "CV_BLUR_NO_SCALE" -> ImgprocColor("CV_BLUR_NO_SCALE", Imgproc.CV_BLUR_NO_SCALE),
     "CV_CANNY_L2_GRADIENT" -> ImgprocColor("CV_CANNY_L2_GRADIENT", Imgproc.CV_CANNY_L2_GRADIENT),
     "CV_CHAIN_CODE" -> ImgprocColor("CV_CHAIN_CODE", Imgproc.CV_CHAIN_CODE),
     "CV_CLOCKWISE" -> ImgprocColor("CV_CLOCKWISE", Imgproc.CV_CLOCKWISE),
     "CV_COMP_BHATTACHARYYA" -> ImgprocColor("CV_COMP_BHATTACHARYYA", Imgproc.CV_COMP_BHATTACHARYYA),
     "CV_COMP_CHISQR" -> ImgprocColor("CV_COMP_CHISQR", Imgproc.CV_COMP_CHISQR),
     "CV_COMP_CORREL" -> ImgprocColor("CV_COMP_CORREL", Imgproc.CV_COMP_CORREL),
     "CV_COMP_HELLINGER" -> ImgprocColor("CV_COMP_HELLINGER", Imgproc.CV_COMP_HELLINGER),
     "CV_COMP_INTERSECT" -> ImgprocColor("CV_COMP_INTERSECT", Imgproc.CV_COMP_INTERSECT),
     "CV_CONTOURS_MATCH_I1" -> ImgprocColor("CV_CONTOURS_MATCH_I1", Imgproc.CV_CONTOURS_MATCH_I1),
     "CV_CONTOURS_MATCH_I2" -> ImgprocColor("CV_CONTOURS_MATCH_I2", Imgproc.CV_CONTOURS_MATCH_I2),
     "CV_CONTOURS_MATCH_I3" -> ImgprocColor("CV_CONTOURS_MATCH_I3", Imgproc.CV_CONTOURS_MATCH_I3),
     "CV_COUNTER_CLOCKWISE" -> ImgprocColor("CV_COUNTER_CLOCKWISE", Imgproc.CV_COUNTER_CLOCKWISE),
     "CV_DIST_C" -> ImgprocColor("CV_DIST_C", Imgproc.CV_DIST_C),
     "CV_DIST_FAIR" -> ImgprocColor("CV_DIST_FAIR", Imgproc.CV_DIST_FAIR),
     "CV_DIST_HUBER" -> ImgprocColor("CV_DIST_HUBER", Imgproc.CV_DIST_HUBER),
     "CV_DIST_L1" -> ImgprocColor("CV_DIST_L1", Imgproc.CV_DIST_L1),
     "CV_DIST_L12" -> ImgprocColor("CV_DIST_L12", Imgproc.CV_DIST_L12),
     "CV_DIST_L2" -> ImgprocColor("CV_DIST_L2", Imgproc.CV_DIST_L2),
     "CV_DIST_LABEL_CCOMP" -> ImgprocColor("CV_DIST_LABEL_CCOMP", Imgproc.CV_DIST_LABEL_CCOMP),
     "CV_DIST_LABEL_PIXEL" -> ImgprocColor("CV_DIST_LABEL_PIXEL", Imgproc.CV_DIST_LABEL_PIXEL),
     "CV_DIST_MASK_3" -> ImgprocColor("CV_DIST_MASK_3", Imgproc.CV_DIST_MASK_3),
     "CV_DIST_MASK_5" -> ImgprocColor("CV_DIST_MASK_5", Imgproc.CV_DIST_MASK_5),
     "CV_DIST_MASK_PRECISE" -> ImgprocColor("CV_DIST_MASK_PRECISE", Imgproc.CV_DIST_MASK_PRECISE),
     "CV_DIST_USER" -> ImgprocColor("CV_DIST_USER", Imgproc.CV_DIST_USER),
     "CV_DIST_WELSCH" -> ImgprocColor("CV_DIST_WELSCH", Imgproc.CV_DIST_WELSCH),
     "CV_GAUSSIAN" -> ImgprocColor("CV_GAUSSIAN", Imgproc.CV_GAUSSIAN),
     "CV_GAUSSIAN_5x5" -> ImgprocColor("CV_GAUSSIAN_5x5", Imgproc.CV_GAUSSIAN_5x5),
     "CV_HOUGH_GRADIENT" -> ImgprocColor("CV_HOUGH_GRADIENT", Imgproc.CV_HOUGH_GRADIENT),
     "CV_HOUGH_MULTI_SCALE" -> ImgprocColor("CV_HOUGH_MULTI_SCALE", Imgproc.CV_HOUGH_MULTI_SCALE),
     "CV_HOUGH_PROBABILISTIC" -> ImgprocColor("CV_HOUGH_PROBABILISTIC", Imgproc.CV_HOUGH_PROBABILISTIC),
     "CV_HOUGH_STANDARD" -> ImgprocColor("CV_HOUGH_STANDARD", Imgproc.CV_HOUGH_STANDARD),
     "CV_LINK_RUNS" -> ImgprocColor("CV_LINK_RUNS", Imgproc.CV_LINK_RUNS),
     "CV_MAX_SOBEL_KSIZE" -> ImgprocColor("CV_MAX_SOBEL_KSIZE", Imgproc.CV_MAX_SOBEL_KSIZE),
     "CV_MEDIAN" -> ImgprocColor("CV_MEDIAN", Imgproc.CV_MEDIAN),
     "CV_POLY_APPROX_DP" -> ImgprocColor("CV_POLY_APPROX_DP", Imgproc.CV_POLY_APPROX_DP),
     "CV_RGBA2mRGBA" -> ImgprocColor("CV_RGBA2mRGBA", Imgproc.CV_RGBA2mRGBA),
     "CV_SCHARR" -> ImgprocColor("CV_SCHARR", Imgproc.CV_SCHARR),
     "CV_SHAPE_CROSS" -> ImgprocColor("CV_SHAPE_CROSS", Imgproc.CV_SHAPE_CROSS),
     "CV_SHAPE_CUSTOM" -> ImgprocColor("CV_SHAPE_CUSTOM", Imgproc.CV_SHAPE_CUSTOM),
     "CV_SHAPE_ELLIPSE" -> ImgprocColor("CV_SHAPE_ELLIPSE", Imgproc.CV_SHAPE_ELLIPSE),
     "CV_SHAPE_RECT" -> ImgprocColor("CV_SHAPE_RECT", Imgproc.CV_SHAPE_RECT),
     "CV_WARP_FILL_OUTLIERS" -> ImgprocColor("CV_WARP_FILL_OUTLIERS", Imgproc.CV_WARP_FILL_OUTLIERS),
     "CV_WARP_INVERSE_MAP" -> ImgprocColor("CV_WARP_INVERSE_MAP", Imgproc.CV_WARP_INVERSE_MAP),
     "CV_mRGBA2RGBA" -> ImgprocColor("CV_mRGBA2RGBA", Imgproc.CV_mRGBA2RGBA),
     "DIST_LABEL_CCOMP" -> ImgprocColor("DIST_LABEL_CCOMP", Imgproc.DIST_LABEL_CCOMP),
     "DIST_LABEL_PIXEL" -> ImgprocColor("DIST_LABEL_PIXEL", Imgproc.DIST_LABEL_PIXEL),
     "FLOODFILL_FIXED_RANGE" -> ImgprocColor("FLOODFILL_FIXED_RANGE", Imgproc.FLOODFILL_FIXED_RANGE),
     "FLOODFILL_MASK_ONLY" -> ImgprocColor("FLOODFILL_MASK_ONLY", Imgproc.FLOODFILL_MASK_ONLY),
     "GC_BGD" -> ImgprocColor("GC_BGD", Imgproc.GC_BGD),
     "GC_EVAL" -> ImgprocColor("GC_EVAL", Imgproc.GC_EVAL),
     "GC_FGD" -> ImgprocColor("GC_FGD", Imgproc.GC_FGD),
     "GC_INIT_WITH_MASK" -> ImgprocColor("GC_INIT_WITH_MASK", Imgproc.GC_INIT_WITH_MASK),
     "GC_INIT_WITH_RECT" -> ImgprocColor("GC_INIT_WITH_RECT", Imgproc.GC_INIT_WITH_RECT),
     "GC_PR_BGD" -> ImgprocColor("GC_PR_BGD", Imgproc.GC_PR_BGD),
     "GC_PR_FGD" -> ImgprocColor("GC_PR_FGD", Imgproc.GC_PR_FGD),
     "GHT_POSITION" -> ImgprocColor("GHT_POSITION", Imgproc.GHT_POSITION),
     "GHT_ROTATION" -> ImgprocColor("GHT_ROTATION", Imgproc.GHT_ROTATION),
     "GHT_SCALE" -> ImgprocColor("GHT_SCALE", Imgproc.GHT_SCALE),
     "INTER_AREA" -> ImgprocColor("INTER_AREA", Imgproc.INTER_AREA),
     "INTER_BITS" -> ImgprocColor("INTER_BITS", Imgproc.INTER_BITS),
     "INTER_BITS2" -> ImgprocColor("INTER_BITS2", Imgproc.INTER_BITS2),
     "INTER_CUBIC" -> ImgprocColor("INTER_CUBIC", Imgproc.INTER_CUBIC),
     "INTER_LANCZOS4" -> ImgprocColor("INTER_LANCZOS4", Imgproc.INTER_LANCZOS4),
     "INTER_LINEAR" -> ImgprocColor("INTER_LINEAR", Imgproc.INTER_LINEAR),
     "INTER_MAX" -> ImgprocColor("INTER_MAX", Imgproc.INTER_MAX),
     "INTER_NEAREST" -> ImgprocColor("INTER_NEAREST", Imgproc.INTER_NEAREST),
     "INTER_TAB_SIZE" -> ImgprocColor("INTER_TAB_SIZE", Imgproc.INTER_TAB_SIZE),
     "INTER_TAB_SIZE2" -> ImgprocColor("INTER_TAB_SIZE2", Imgproc.INTER_TAB_SIZE2),
     "KERNEL_ASYMMETRICAL" -> ImgprocColor("KERNEL_ASYMMETRICAL", Imgproc.KERNEL_ASYMMETRICAL),
     "KERNEL_GENERAL" -> ImgprocColor("KERNEL_GENERAL", Imgproc.KERNEL_GENERAL),
     "KERNEL_INTEGER" -> ImgprocColor("KERNEL_INTEGER", Imgproc.KERNEL_INTEGER),
     "KERNEL_SMOOTH" -> ImgprocColor("KERNEL_SMOOTH", Imgproc.KERNEL_SMOOTH),
     "KERNEL_SYMMETRICAL" -> ImgprocColor("KERNEL_SYMMETRICAL", Imgproc.KERNEL_SYMMETRICAL),
     "MORPH_BLACKHAT" -> ImgprocColor("MORPH_BLACKHAT", Imgproc.MORPH_BLACKHAT),
     "MORPH_CLOSE" -> ImgprocColor("MORPH_CLOSE", Imgproc.MORPH_CLOSE),
     "MORPH_CROSS" -> ImgprocColor("MORPH_CROSS", Imgproc.MORPH_CROSS),
     "MORPH_DILATE" -> ImgprocColor("MORPH_DILATE", Imgproc.MORPH_DILATE),
     "MORPH_ELLIPSE" -> ImgprocColor("MORPH_ELLIPSE", Imgproc.MORPH_ELLIPSE),
     "MORPH_ERODE" -> ImgprocColor("MORPH_ERODE", Imgproc.MORPH_ERODE),
     "MORPH_GRADIENT" -> ImgprocColor("MORPH_GRADIENT", Imgproc.MORPH_GRADIENT),
     "MORPH_OPEN" -> ImgprocColor("MORPH_OPEN", Imgproc.MORPH_OPEN),
     "MORPH_RECT" -> ImgprocColor("MORPH_RECT", Imgproc.MORPH_RECT),
     "MORPH_TOPHAT" -> ImgprocColor("MORPH_TOPHAT", Imgproc.MORPH_TOPHAT),
     "PROJ_SPHERICAL_EQRECT" -> ImgprocColor("PROJ_SPHERICAL_EQRECT", Imgproc.PROJ_SPHERICAL_EQRECT),
     "PROJ_SPHERICAL_ORTHO" -> ImgprocColor("PROJ_SPHERICAL_ORTHO", Imgproc.PROJ_SPHERICAL_ORTHO),
     "RETR_CCOMP" -> ImgprocColor("RETR_CCOMP", Imgproc.RETR_CCOMP),
     "RETR_EXTERNAL" -> ImgprocColor("RETR_EXTERNAL", Imgproc.RETR_EXTERNAL),
     "RETR_FLOODFILL" -> ImgprocColor("RETR_FLOODFILL", Imgproc.RETR_FLOODFILL),
     "RETR_LIST" -> ImgprocColor("RETR_LIST", Imgproc.RETR_LIST),
     "RETR_TREE" -> ImgprocColor("RETR_TREE", Imgproc.RETR_TREE),
     "THRESH_BINARY" -> ImgprocColor("THRESH_BINARY", Imgproc.THRESH_BINARY),
     "THRESH_BINARY_INV" -> ImgprocColor("THRESH_BINARY_INV", Imgproc.THRESH_BINARY_INV),
     "THRESH_MASK" -> ImgprocColor("THRESH_MASK", Imgproc.THRESH_MASK),
     "THRESH_OTSU" -> ImgprocColor("THRESH_OTSU", Imgproc.THRESH_OTSU),
     "THRESH_TOZERO" -> ImgprocColor("THRESH_TOZERO", Imgproc.THRESH_TOZERO),
     "THRESH_TOZERO_INV" -> ImgprocColor("THRESH_TOZERO_INV", Imgproc.THRESH_TOZERO_INV),
     "THRESH_TRUNC" -> ImgprocColor("THRESH_TRUNC", Imgproc.THRESH_TRUNC),
     "TM_CCOEFF" -> ImgprocColor("TM_CCOEFF", Imgproc.TM_CCOEFF),
     "TM_CCOEFF_NORMED" -> ImgprocColor("TM_CCOEFF_NORMED", Imgproc.TM_CCOEFF_NORMED),
     "TM_CCORR" -> ImgprocColor("TM_CCORR", Imgproc.TM_CCORR),
     "TM_CCORR_NORMED" -> ImgprocColor("TM_CCORR_NORMED", Imgproc.TM_CCORR_NORMED),
     "TM_SQDIFF" -> ImgprocColor("TM_SQDIFF", Imgproc.TM_SQDIFF),
     "TM_SQDIFF_NORMED" -> ImgprocColor("TM_SQDIFF_NORMED", Imgproc.TM_SQDIFF_NORMED),
     "WARP_INVERSE_MAP" -> ImgprocColor("WARP_INVERSE_MAP", Imgproc.WARP_INVERSE_MAP),
     "BORDER_ISOLATED" -> ImgprocColor("BORDER_ISOLATED", Imgproc.BORDER_ISOLATED),
     "COLOR_BGR2BGR555" -> ImgprocColor("COLOR_BGR2BGR555", Imgproc.COLOR_BGR2BGR555),
     "COLOR_BGR2BGR565" -> ImgprocColor("COLOR_BGR2BGR565", Imgproc.COLOR_BGR2BGR565),
     "COLOR_BGR5552BGR" -> ImgprocColor("COLOR_BGR5552BGR", Imgproc.COLOR_BGR5552BGR),
     "COLOR_BGR5552BGRA" -> ImgprocColor("COLOR_BGR5552BGRA", Imgproc.COLOR_BGR5552BGRA),
     "COLOR_BGR5552GRAY" -> ImgprocColor("COLOR_BGR5552GRAY", Imgproc.COLOR_BGR5552GRAY),
     "COLOR_BGR5552RGB" -> ImgprocColor("COLOR_BGR5552RGB", Imgproc.COLOR_BGR5552RGB),
     "COLOR_BGR5552RGBA" -> ImgprocColor("COLOR_BGR5552RGBA", Imgproc.COLOR_BGR5552RGBA),
     "COLOR_BGR5652BGR" -> ImgprocColor("COLOR_BGR5652BGR", Imgproc.COLOR_BGR5652BGR),
     "COLOR_BGR5652BGRA" -> ImgprocColor("COLOR_BGR5652BGRA", Imgproc.COLOR_BGR5652BGRA),
     "COLOR_BGR5652GRAY" -> ImgprocColor("COLOR_BGR5652GRAY", Imgproc.COLOR_BGR5652GRAY),
     "COLOR_BGR5652RGB" -> ImgprocColor("COLOR_BGR5652RGB", Imgproc.COLOR_BGR5652RGB),
     "COLOR_BGR5652RGBA" -> ImgprocColor("COLOR_BGR5652RGBA", Imgproc.COLOR_BGR5652RGBA),
     "COLOR_BGRA2BGR555" -> ImgprocColor("COLOR_BGRA2BGR555", Imgproc.COLOR_BGRA2BGR555),
     "COLOR_BGRA2BGR565" -> ImgprocColor("COLOR_BGRA2BGR565", Imgproc.COLOR_BGRA2BGR565),
     "COLOR_BayerBG2BGR" -> ImgprocColor("COLOR_BayerBG2BGR", Imgproc.COLOR_BayerBG2BGR),
     "COLOR_BayerBG2BGR_VNG" -> ImgprocColor("COLOR_BayerBG2BGR_VNG", Imgproc.COLOR_BayerBG2BGR_VNG),
     "COLOR_BayerBG2GRAY" -> ImgprocColor("COLOR_BayerBG2GRAY", Imgproc.COLOR_BayerBG2GRAY),
     "COLOR_BayerBG2RGB" -> ImgprocColor("COLOR_BayerBG2RGB", Imgproc.COLOR_BayerBG2RGB),
     "COLOR_GRAY2BGR555" -> ImgprocColor("COLOR_GRAY2BGR555", Imgproc.COLOR_GRAY2BGR555),
     "COLOR_GRAY2BGR565" -> ImgprocColor("COLOR_GRAY2BGR565", Imgproc.COLOR_GRAY2BGR565),
     "COLOR_RGB2BGR555" -> ImgprocColor("COLOR_RGB2BGR555", Imgproc.COLOR_RGB2BGR555),
     "COLOR_RGB2BGR565" -> ImgprocColor("COLOR_RGB2BGR565", Imgproc.COLOR_RGB2BGR565),
     "COLOR_RGBA2BGR555" -> ImgprocColor("COLOR_RGBA2BGR555", Imgproc.COLOR_RGBA2BGR555),
     "COLOR_RGBA2BGR565" -> ImgprocColor("COLOR_RGBA2BGR565", Imgproc.COLOR_RGBA2BGR565))

  trait OpenCVUtils extends Utils {

    def loadNativeLibs() = {
      val nativeLibName = if (runOnMac)"/opt/local/share/OpenCV/java/libopencv_java244.dylib" else"c:/openCV/build/java/x64/opencv_java244.dll"
      System.load(new File(nativeLibName).getAbsolutePath())
    }

    def mat2Image(mat: Mat): Try[Image] = {
      val memory = new MatOfByte
      try {
    	  	Highgui.imencode(".png", mat, memory)
    	  	Success(new Image(new ByteArrayInputStream(memory.toArray())))
      } catch {
        case e  : Exception => Failure(e)
      }
    }

    def noOp(mat: Mat) = mat

    def colorSpace(colorSpace : => Int)(originalMat: Mat): Mat = {
      val colorTransformed = new Mat
      // for example, grayscale :  Imgproc.COLOR_BGR2GRAY
      try {
    	  	Imgproc.cvtColor(originalMat, colorTransformed, colorSpace)
    	  	colorTransformed
      } catch {
        case e => originalMat
      }
    }

    def blur(size: Size)(input: Mat): Mat = {
      val blurredMat = new Mat
      Imgproc.blur(input, blurredMat, size)
      blurredMat
    }

    def chop(height: => Int, width: => Int)(input: Mat): Mat = new Mat(input, new Range(1, height), new Range(1, width))

  }

  trait ImageSource {

    def videoCapture: VideoCapture

    def takeImage: Mat = {
      val image = new Mat()
      while (videoCapture.read(image) == false) {
        Thread.sleep(1)
        println("waiting for camera ...")
      }
      image
    }

    def sourceMat: Try[Mat] = {
      assert(videoCapture.isOpened())
      if (videoCapture.grab) {
        Success(takeImage)
      } else {
        Failure(new RuntimeException("Couldn't grab image!"))
      }
    }

  }

  trait FaceScanner {

    def faceDetector: CascadeClassifier

    def scanFace(image: Mat): Mat = {

      // Detect faces in the image.
      // MatOfRect is a special container class for Rect.
      val faceDetections = new MatOfRect()
      faceDetector.detectMultiScale(image, faceDetections)

      // Draw a bounding box around each face.
      for (rect <- faceDetections.toArray()) {
        Core.rectangle(image, new Point(rect.x, rect.y), new Point(rect.x + rect.width, rect.y + rect.height), new Scalar(0, 255, 0))
      }

      image
    }

  }
}

class WebcamService extends Service[Try[Mat]] with OpenCVUtils with JfxUtils with ImageSource {

  val videoCapture: VideoCapture = new VideoCapture(0)

  def createTask = mkTask(sourceMat)

}

object OpenCVWithJavaFX {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[OpenCVWithJavaFX], args: _*)
  }

}

trait JfxUtils {

  def mkCellFactoryCallback[T](listCellGenerator: ListView[T] => ListCell[T]) = new Callback[ListView[T], ListCell[T]]() {
    override def call(list: ListView[T]): ListCell[T] = listCellGenerator(list)
  }

  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }
  def mkTask[X](callFn: => X): Task[X] = new Task[X] { override def call(): X = callFn }

  def mkTop: HBox = {
    val hbox = new HBox()
    hbox.setStyle("-fx-padding: 15;" +
     "-fx-background-color: #333333," +
     "linear-gradient(#f3f3f3 0%, #ced3da 100%);" +
     "-fx-background-insets: 0, 0 0 1 0;")
    hbox
  }

  def mkSlider(min: Int, max: Int, initialValue: Int, orientation: Orientation): Slider = {
    require(min <= initialValue)
    require(initialValue <= max)
    val slider = new Slider()
    slider.setMin(min)
    slider.setMax(max)
    slider.setValue(initialValue)
    slider.setShowTickLabels(true)
    slider.setShowTickMarks(true)
    slider.setMajorTickUnit(100)
    slider.setMinorTickCount(20)
    slider.setBlockIncrement(1)
    slider.setOrientation(orientation)
    slider
  }

}

class OpenCVWithJavaFX extends javafx.application.Application with FaceScanner with OpenCVUtils with Utils with JfxUtils {

  // Create a face detector from the cascade file in the resources directory.
  lazy val faceDetector: CascadeClassifier = new CascadeClassifier(getClass().getResource("/lbpcascade_frontalface.xml").getPath())

  override def init(): Unit = loadNativeLibs // important to have this statement on the "right" thread

  def toggleOp(b: ToggleButton, mat: Mat)(left: Mat => Mat, right: Mat => Mat): Mat = if (b.isSelected()) left(mat) else right(mat)

  val imageProperty = new SimpleObjectProperty[Image]()
  def setImage(image: Image) = imageProperty.set(image)
  def getImage(): Image = imageProperty.get

//  val MaxWidth = 1280
//  val MaxHeight = 720
  val MaxWidth = 1024
  val MaxHeight = 720
  
  override def start(stage: Stage): Unit = {
    val imageService = new WebcamService
    stage.setTitle("Webcam snapshot with face detection")
    val bp = new BorderPane
    val imageView = new ImageView()
    imageView.imageProperty().bind(imageProperty)
    val label = new Label()
    val imageBp = new BorderPane

    label.fontProperty().setValue(Font.font("Verdana", 80))
    val widthSlider = mkSlider(2, MaxWidth, MaxWidth, Orientation.HORIZONTAL)
    val heightSlider = mkSlider(2, MaxHeight, MaxHeight, Orientation.VERTICAL)
    def chopWithSliders = chop(heightSlider.getValue.toInt, widthSlider.getValue.toInt)_

    val chopToggle = new ToggleButton("Activate Chopping")
    chopToggle.setSelected(true)
    heightSlider.disableProperty.bind(chopToggle.selectedProperty.not)
    widthSlider.disableProperty.bind(chopToggle.selectedProperty.not)

    val scanFaceToggle = new ToggleButton("with face recognition")
    val colorToggle = new ToggleButton("convert to other color space")

    val colorComboBox = {
      class ImgprocColorCell extends ListCell[ImgprocColor] {

        override def updateItem(item: ImgprocColor, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          if (item != null) {
            setText(item.id)
          }
        }

      }
      val cb = new ComboBox[ImgprocColor]
      val sortedItems = colorConstants.values.toSeq.sortWith((a, b) => a.id < b.id)
      cb.getItems.addAll(sortedItems)
      cb.setValue(sortedItems.head)
      cb.setCellFactory(mkCellFactoryCallback(lv => new ImgprocColorCell))
      cb.setOnAction(mkEventHandler(e => println(cb.getValue())))
      cb.visibleProperty.bind(colorToggle.selectedProperty)
      cb
    }

    val blurSlider = mkSlider(1, 100, 10, Orientation.HORIZONTAL)
    val blurToggle = new ToggleButton("blur")
    blurSlider.visibleProperty().bind(blurToggle.selectedProperty)
    val topBox = mkTop
    topBox.getChildren.addAll(chopToggle, colorToggle, colorComboBox, blurToggle, blurSlider, scanFaceToggle)
    imageBp.setCenter(imageView)
    imageBp.setRight(heightSlider)
    imageBp.setBottom(widthSlider)
    bp.setTop(topBox)
    bp.setCenter(imageBp)
    bp.setBottom(label)
    val scene = new Scene(bp, MaxWidth + 100, MaxHeight + 300)
    stage.setScene(scene)

    imageService.setOnSucceeded(
      mkEventHandler(
        event =>
          either(event.getSource().getValue.asInstanceOf[Either[Exception, Mat]])(
            e => println(e.getMessage),
            fromCamera => {
              val old = System.currentTimeMillis()
              val choppedMat = toggleOp(chopToggle, fromCamera)(chopWithSliders, noOp)
              val bwFiltered = toggleOp(colorToggle, choppedMat)(colorSpace(colorComboBox.getValue.value), noOp)
              val blurred = 
                toggleOp(blurToggle, bwFiltered)(
                    blur(new Size(blurSlider.getValue.toInt, blurSlider.getValue.toInt)), noOp)
              val faceDetected = toggleOp(scanFaceToggle, blurred)(scanFace, noOp)
              attempt(mat2Image(faceDetected))(e => println(e.getMessage), image => setImage(image))
              val time = (System.currentTimeMillis() - old)
              label.textProperty.set("%s ms".format(time))
              imageService.restart
            })))

    imageService.start
    stage.show()

  }

}