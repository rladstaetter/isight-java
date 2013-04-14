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

/**
 * see also http://ladstatt.blogspot.com/
 */
object HelloOpenCVUsingIsight {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[HelloOpenCVUsingIsight], args: _*)
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

class HelloOpenCVUsingIsight extends javafx.application.Application with FaceScanner with OpenCVUtils {

  // Create a face detector from the cascade file in the resources directory.
  var faceDetector: CascadeClassifier = _

  val runOnMac =
    {
      System.getProperty("os.name").toLowerCase match {
        case "mac os x" => true
        case _ => false
      }
    }

  val nativeLibName = if (runOnMac) "/opt/local/share/OpenCV/java/libopencv_java244.dylib" else "c:/openCV/build/java/opencv-244.jar"

  override def init(): Unit = {
    // important to have this statement on the "right" thread
    System.load(new File(nativeLibName).getAbsolutePath())
    faceDetector = new CascadeClassifier(getClass().getResource("/lbpcascade_frontalface.xml").getPath())
  }

  val imageProperty = new SimpleObjectProperty[Image]()
  def setImage(image: Image) = imageProperty.set(image)
  def getImage(): Image = imageProperty.get

  val converter = new IntegerStringConverter().asInstanceOf[StringConverter[Long]]

  def mkTop(toggleButton: ToggleButton): HBox = {
    val hbox = new HBox()
    hbox.getChildren().add(toggleButton)
    hbox.setStyle("-fx-padding: 15;" +
      "-fx-background-color: #333333, " +
      "linear-gradient(#f3f3f3 0%, #ced3da 100%);" +
      "-fx-background-insets: 0, 0 0 1 0;")
    hbox
  }

  override def start(stage: Stage): Unit = {
    val imageService = new WebcamService
    stage.setTitle("Webcam snapshot with face detection")
    val bp = new BorderPane
    val imageView = new ImageView()
    val label = new Label()
    label.fontProperty().setValue(Font.font("Verdana", 80))
    val toggleBtn = new ToggleButton("with face recognition")
    imageView.imageProperty().bind(imageProperty)
    bp.setTop(mkTop(toggleBtn))
    bp.setCenter(imageView)
    bp.setBottom(label)
    val scene = new Scene(bp, 1280, 920)
    stage.setScene(scene)

    imageService.setOnSucceeded(new EventHandler[WorkerStateEvent] {
      override def handle(event: WorkerStateEvent) = {
        event.getSource().getValue match {
          case Left(e: RuntimeException) => println(e.getMessage)
          case Right(mat: Mat) => {
            val old = System.currentTimeMillis()
            setImage(mat2Image(if (toggleBtn.isSelected()) scanFace(mat) else mat))
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