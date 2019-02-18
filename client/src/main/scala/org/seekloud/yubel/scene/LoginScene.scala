package org.seekloud.yubel.scene

import org.seekloud.yubel.Boot
import java.io.ByteArrayInputStream

import javafx.event.{Event, EventHandler}
import javafx.scene.{Group, Scene}
import javafx.scene.canvas.Canvas
import javafx.scene.control.Button
import javafx.scene.effect.DropShadow
import javafx.scene.image.{Image, ImageView}
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.text.Font

/**
  * Created by dry on 2018/10/29.
  **/
object LoginScene {
  trait LoginSceneListener {
    def loginByMail()
    def comeBack()
  }
}

class LoginScene {
  import LoginScene._

  val width = 500
  val height = 500
  val group = new Group
  val button = new Button("邮箱登录")
  val canvas = new Canvas(width, height)
  val canvasCtx = canvas.getGraphicsContext2D
  var loginSceneListener: LoginSceneListener = _

  val img = new Image("img/Paper-Io-Online.jpg")
  Boot.addToPlatform(canvasCtx.drawImage(img, 0, 0 ,500 ,500))

  button.setLayoutX(212)
  button.setLayoutY(380)
  val shadow = new DropShadow()

  button.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      button.setEffect(shadow)
    }
  })

  button.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      button.setEffect(null)
    }
  })

  button.setStyle("-fx-font: 15 arial; -fx-base: #67B567; -fx-background-radius: 10px;") //green

  val backBtn = new Button("返回")
  backBtn.setLayoutX(228)
  backBtn.setLayoutY(420)
  backBtn.setStyle("-fx-font: 15 arial; -fx-base: #CA5C54; -fx-background-radius: 10px;")

  canvasCtx.setFill(Color.rgb(255, 255, 255))
  canvasCtx.fillRect(0, 0, width, height)
  canvasCtx.setFont(Font.font(18))
  canvasCtx.setFill(Color.BLACK)
  canvasCtx.fillText("扫码登录", 215, 380)
  group.getChildren.add(canvas)
  group.getChildren.add(button)
  group.getChildren.add(backBtn)
  val scene = new Scene(group)

  button.setOnAction(_ => loginSceneListener.loginByMail())
  backBtn.setOnAction(_ => loginSceneListener.comeBack())

  def drawScanUrl(imageStream: ByteArrayInputStream) = {
    Boot.addToPlatform {
//      group.getChildren.remove(button)
      val img = new Image(imageStream)
      canvasCtx.drawImage(img,100,40)
    }
  }

  def setLoginSceneListener(listener: LoginSceneListener) {
    loginSceneListener = listener
  }

}
