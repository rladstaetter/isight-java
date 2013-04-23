package net.ladstatt.apps.isight

import java.io.ByteArrayInputStream
import java.io.File
import org.opencv.core.Core
import org.opencv.core.Mat
import org.opencv.core.MatOfByte
import org.opencv.core.MatOfRect
import org.opencv.core.Point
import org.opencv.core.Scalar
import org.opencv.highgui.Highgui
import org.opencv.highgui.VideoCapture
import org.opencv.objdetect.CascadeClassifier
import javafx.application.Application
import javafx.beans.property.SimpleObjectProperty
import javafx.concurrent.Service
import javafx.concurrent.Task
import javafx.concurrent.WorkerStateEvent
import javafx.event.EventHandler
import javafx.scene.Scene
import javafx.scene.control.Label
import javafx.scene.control.ToggleButton
import javafx.scene.image.Image
import javafx.scene.image.ImageView
import javafx.scene.layout.BorderPane
import javafx.scene.layout.HBox
import javafx.scene.text.Font
import javafx.stage.Stage
import javafx.util.StringConverter
import javafx.util.converter.IntegerStringConverter
import org.opencv.core.Range
import javafx.scene.control.Slider
import javafx.geometry.Orientation
import org.opencv.imgproc.Imgproc

/**
 * see also http://ladstatt.blogspot.com/
 */
object OpenCVWithJavaFX {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[OpenCVWithJavaFX], args: _*)
  }
}

trait OpenCVUtils {

  def mat2Image(mat: Mat): Image = {
    val memory = new MatOfByte
    Highgui.imencode(".png", mat, memory)
    new Image(new ByteArrayInputStream(memory.toArray()))
  }

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

  def sourceMat: Either[Exception, Mat] = {
    assert(videoCapture.isOpened())
    if (videoCapture.grab) {
      Right(takeImage)
    } else {
      Left(new RuntimeException("Couldn't grab image!"))
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

class WebcamService extends Service[Either[Exception, Mat]] with OpenCVUtils with ImageSource {

  val videoCapture: VideoCapture = new VideoCapture(0)

  def createTask(): Task[Either[Exception, Mat]] = {
    new Task[Either[Exception, Mat]] {
      override def call(): Either[Exception, Mat] = sourceMat
    }
  }

}

class OpenCVWithJavaFX extends javafx.application.Application with FaceScanner with OpenCVUtils {

  // Create a face detector from the cascade file in the resources directory.
  var faceDetector: CascadeClassifier = _

  val runOnMac =
    {
      System.getProperty("os.name").toLowerCase match {
        case "mac os x" => true
        case _ => false
      }
    }

  val nativeLibName = if (runOnMac) "/opt/local/share/OpenCV/java/libopencv_java244.dylib" else "c:/openCV/build/java/x86/opencv_java244.dll"

  override def init(): Unit = {
    // important to have this statement on the "right" thread
    System.load(new File(nativeLibName).getAbsolutePath())
    faceDetector = new CascadeClassifier(getClass().getResource("/lbpcascade_frontalface.xml").getPath())
  }

  val imageProperty = new SimpleObjectProperty[Image]()
  def setImage(image: Image) = imageProperty.set(image)
  def getImage(): Image = imageProperty.get

  val converter = new IntegerStringConverter().asInstanceOf[StringConverter[Long]]

  def mkTop: HBox = {
    val hbox = new HBox()
    hbox.setStyle("-fx-padding: 15;" +
      "-fx-background-color: #333333, " +
      "linear-gradient(#f3f3f3 0%, #ced3da 100%);" +
      "-fx-background-insets: 0, 0 0 1 0;")
    hbox
  }

  def mkSlider(min: Int, max: Int, orientation: Orientation): Slider = {
    val slider = new Slider()
    slider.setMin(min)
    slider.setMax(max)
    slider.setValue(max / 2)
    slider.setShowTickLabels(true)
    slider.setShowTickMarks(true)
    slider.setMajorTickUnit(100)
    slider.setMinorTickCount(20)
    slider.setBlockIncrement(1)
    slider.setOrientation(orientation)
    slider
  }

  val MaxWidth = 1280
  val MaxHeight = 720

  override def start(stage: Stage): Unit = {
    val imageService = new WebcamService
    stage.setTitle("Webcam snapshot with face detection")
    val bp = new BorderPane
    val imageView = new ImageView()
    val label = new Label()
    val imageBp = new BorderPane

    label.fontProperty().setValue(Font.font("Verdana", 80))
    val widthSlider = mkSlider(2, MaxWidth, Orientation.HORIZONTAL)
    val heightSlider = mkSlider(2, MaxHeight, Orientation.VERTICAL)
    val toggleBtn = new ToggleButton("with face recognition")
    imageView.imageProperty().bind(imageProperty)
    val topBox = mkTop
    topBox.getChildren.addAll(toggleBtn)
    imageBp.setCenter(imageView)
    imageBp.setRight(heightSlider)
    imageBp.setBottom(widthSlider)
    bp.setTop(topBox)
    bp.setCenter(imageBp)
    bp.setBottom(label)
    val scene = new Scene(bp, 1280, 920)
    stage.setScene(scene)

    def convert2Gray(originalMat: Mat): Mat = {
      val grayMat = new Mat
      Imgproc.cvtColor(originalMat, grayMat, Imgproc.COLOR_BGR2GRAY)
      grayMat
    }

    imageService.setOnSucceeded(new EventHandler[WorkerStateEvent] {
      override def handle(event: WorkerStateEvent) = {
        event.getSource().getValue match {
          case Left(e: RuntimeException) => println(e.getMessage)
          case Right(mat: Mat) => {
            val choppedMat = new Mat(mat, new Range(1, heightSlider.getValue.toInt), new Range(1, widthSlider.getValue.toInt))
            val grayMat = convert2Gray(choppedMat)
            val old = System.currentTimeMillis()
            setImage(mat2Image(if (toggleBtn.isSelected()) scanFace(grayMat) else grayMat))
            val time = (System.currentTimeMillis() - old)
            label.textProperty.set("%s ms".format(time))
            imageService.restart
          }
        }
      }
    })
    imageService.start
    stage.show()

  }

}