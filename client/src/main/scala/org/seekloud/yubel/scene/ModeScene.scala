package org.seekloud.yubel.scene

import javafx.event.EventHandler
import javafx.scene.{Group, Scene}
import javafx.scene.control.Button
import javafx.scene.effect.DropShadow
import javafx.scene.image.{Image, ImageView}
import javafx.scene.input.MouseEvent

//abstract class ModeSceneListener {
//  def gotoNormalScene()
//  def gotoBotScene()
//}

trait ModeSceneListener {
  def gotoNormalScene()
  def gotoBotScene()
}

class ModeScene {
  val width = 500
  val height = 500
  private val group = new Group()
  private val scene = new Scene(group, width, height)

  val img = new ImageView("img/Paper-Io-Online.jpg")
  img.setFitWidth(500)
  img.setFitHeight(500)
  group.getChildren.add(img)

  val imgUser = new ImageView("img/user.png")
  imgUser.setFitWidth(10)
  imgUser.setFitHeight(10)

  val imgBot = new ImageView("img/robot.png")
  imgBot.setFitWidth(10)
  imgBot.setFitHeight(10)

  val normalBtn = new Button("普通模式", imgUser)
  val botBtn = new Button("Bot模式", imgBot)

  normalBtn.setLayoutX(205)
  normalBtn.setLayoutY(150)
  botBtn.setLayoutX(205)
  botBtn.setLayoutY(280)

  normalBtn.setStyle("-fx-font: 15 arial; -fx-base: #307CF0; -fx-background-radius: 10px;") //blue
  botBtn.setStyle("-fx-font: 15 arial; -fx-base: #307CF0; -fx-background-radius: 10px;")

  val shadow = new DropShadow()

  normalBtn.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      normalBtn.setEffect(shadow)
    }
  })

  normalBtn.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      normalBtn.setEffect(null)
    }
  })

  botBtn.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      botBtn.setEffect(shadow)
    }
  })

  botBtn.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      botBtn.setEffect(null)
    }
  })



  def getScene = this.scene

  var listener:ModeSceneListener = _

  group.getChildren.addAll(normalBtn,botBtn)

  normalBtn.setOnAction(_ => listener.gotoNormalScene())
  botBtn.setOnAction(_ => listener.gotoBotScene())

  def setListener(listener: ModeSceneListener):Unit = {
    this.listener = listener
  }
}
