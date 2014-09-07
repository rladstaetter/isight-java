package net.ladstatt.apps.isight

import java.io.ByteArrayInputStream
import java.io.File
import scala.collection.JavaConversions.seqAsJavaList
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
import scala.concurrent._
import ExecutionContext.Implicits.global
import OpenCV._
import javafx.application.Platform

/**
 * For a discussion of the concepts of this application see http://ladstatt.blogspot.com/
 */
trait Utils {

  lazy val runOnMac = {
    System.getProperty("os.name").toLowerCase match {
      case "mac os x" => true
      case _ => false
    }
  }

  /**
   * function to measure execution time of first function, optionally executing a display function,
   * returning the time in milliseconds
   */
  def time[A](a: => A, display: Long => Unit = s => ()): A = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    display(micros)
    result
  }

}

object OpenCV {

  case class ImgprocColor(val id: String, value: Int) {
    override def toString = id
  }

  val colorConstants =
    Map(
      "COLOR_BGR2GRAY" -> ImgprocColor("COLOR_BGR2GRAY", Imgproc.COLOR_BGR2GRAY),
      "COLOR_BGR2RGB" -> ImgprocColor("COLOR_BGR2RGB", Imgproc.COLOR_BGR2RGB),
      "COLOR_BGR2Luv" -> ImgprocColor("COLOR_BGR2Luv", Imgproc.COLOR_BGR2Luv))

  trait OpenCVUtils extends Utils {

    def loadNativeLibs() = {
      val nativeLibName = if (runOnMac) "/Users/lad/Documents/net.ladstatt/opencv/src/main/lib/mac/libopencv_java246.dylib" else "c:/openCV/build/java/x64/opencv_java244.dll"
      System.load(new File(nativeLibName).getAbsolutePath())
    }

    def mat2Image(mat: Mat): Future[Image] = {
      Future {
        val memory = new MatOfByte
        Highgui.imencode(".png", mat, memory)
        new Image(new ByteArrayInputStream(memory.toArray()))
      }
    }

    def colorSpace(enabled: => Boolean)(colorSpace: => Int)(input: Mat): Future[Mat] = {
      Future {
        if (enabled) {
          val colorTransformed = new Mat
          Imgproc.cvtColor(input, colorTransformed, colorSpace)
          colorTransformed
        } else input
      } recover {
        case x => input
      }
    }

    def blur(enabled: => Boolean)(size: Size)(input: Mat): Future[Mat] = {
      Future {
        if (enabled) {
          val blurredMat = new Mat
          Imgproc.blur(input, blurredMat, size)
          blurredMat
        } else {
          input
        }
      } recover {
        case e => input
      }
    }

    def chop(enabled: => Boolean)(height: => Int, width: => Int)(input: Mat): Future[Mat] =
      Future {
        if (enabled) {
          new Mat(input, new Range(1, height), new Range(1, width))
        } else input
      } recover {
        case e => input
      }
  }

  trait ImageSource {

    def videoCapture: VideoCapture

    def takeImage: Mat = {
      val image = new Mat()
      while (videoCapture.read(image) == false) {}
      image
    }

    def sourceMat: Future[Mat] =
      Future {
        assert(videoCapture.isOpened())
        if (videoCapture.grab) {
          takeImage
        } else
          throw new RuntimeException("Couldn't grab image!")
      }

  }

  trait FaceScanner {

    def faceDetector: CascadeClassifier

    def scanFace(enabled: => Boolean)(input: Mat): Future[Mat] = {
      Future {
        if (enabled) {
          val faceDetections = new MatOfRect()
          faceDetector.detectMultiScale(input, faceDetections)
          for (rect <- faceDetections.toArray()) {
            Core.rectangle(input, new Point(rect.x, rect.y), new Point(rect.x + rect.width.toDouble, rect.y + rect.height.toDouble), new Scalar(0, 255, 0))
          }
          input
        } else input
      } recover {
        case e => input
      }
    }
  }

}

class WebcamService extends Service[Future[Mat]] with OpenCVUtils with JfxUtils with ImageSource {

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

  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] {
    def handle(e: E) = f(e)
  }

  def mkTask[X](callFn: => X): Task[X] = new Task[X] {
    override def call(): X = callFn
  }

  def mkTop: HBox = {
    val hbox = new HBox()
    hbox.setStyle("-fx-padding: 15;" +
      "-fx-background-color: #333333," +
      "linear-gradient(#f3f3f3 0%, #ced3da 100%);" +
      "-fx-background-insets: 0, 0 0 1 0;")
    hbox
  }

  def mkSlider(min: Double, max: Double, initialValue: Double, orientation: Orientation): Slider = {
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

  def execOnUIThread(f: => Unit) {
    Platform.runLater(new Runnable {
      override def run() = f
    })
  }

  // Create a face detector from the cascade file in the resources directory.
  lazy val faceDetector: CascadeClassifier = new CascadeClassifier(getClass().getResource("/lbpcascade_frontalface.xml").getPath())

  override def init(): Unit = loadNativeLibs // important to have this statement on the "right" thread

  def toggleOp(b: ToggleButton, mat: Mat)(left: Mat => Mat, right: Mat => Mat): Mat =
    if (b.isSelected()) left(mat) else right(mat)

  val imageProperty = new SimpleObjectProperty[Image]()

  def setImage(image: Image) = execOnUIThread(imageProperty.set(image))

  def getImage(): Image = imageProperty.get

  val MaxWidth = 1024.0
  val MaxHeight = 720.0

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

    val chopToggle = new ToggleButton("Activate Chopping")
    def chopWithSliders = chop(chopToggle.isSelected)(heightSlider.getValue.toInt, widthSlider.getValue.toInt) _
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
        event => {
          time(
            for {
              fromCamera <- event.getSource.getValue.asInstanceOf[Future[Mat]]
              chopped <- chopWithSliders(fromCamera)
              colorspaced <- colorSpace(colorToggle.isSelected)(colorComboBox.getValue.value)(chopped)
              blurred <- blur(blurToggle.isSelected)(new Size(blurSlider.getValue, blurSlider.getValue))(colorspaced)
              faced <- scanFace(scanFaceToggle.isSelected)(blurred)
              image <- mat2Image(faced)
            } {
              setImage(image)
              Platform.runLater(
                new Runnable() {
                  def run = {
                    imageService.restart
                  }
                })
            }, time =>
              label.textProperty.set("%d ms".format(time)))
        }))

    imageService.start
    stage.show()

  }

}