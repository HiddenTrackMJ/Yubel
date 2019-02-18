package org.seekloud.yubel.scene

import javafx.event.EventHandler
import javafx.geometry.{Insets, Pos}
import javafx.scene.{Group, Scene}
import javafx.scene.control.{Button, Label, PasswordField, TextField}
import javafx.scene.effect.DropShadow
import javafx.scene.image.ImageView
import javafx.scene.input.MouseEvent
import javafx.scene.layout.{GridPane, HBox, VBox}

abstract class BotSceneListener {
  def confirm(botId:String,botKey:String)
  def comeBack()
}

class BotScene {
  val width = 500
  val height = 500
  val group = new Group()
  private val scene = new Scene(group, width, height)

  val img = new ImageView("img/Paper-Io-Online.jpg")
  img.setFitWidth(500)
  img.setFitHeight(500)
  group.getChildren.add(img)

  val confirm = new Button("确定")
  confirm.setStyle("-fx-font: 15 arial; -fx-base: #67B567; -fx-background-radius: 10px;")
  val backBtn = new Button("返回")
  backBtn.setStyle("-fx-font: 15 arial; -fx-base: #CA5C54; -fx-background-radius: 10px;")//red

  val shadow = new DropShadow()

  confirm.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      confirm.setEffect(shadow)
    }
  })

  confirm.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      confirm.setEffect(null)
    }
  })

  backBtn.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      backBtn.setEffect(shadow)
    }
  })

  backBtn.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      backBtn.setEffect(null)
    }
  })

  confirm.setLayoutX(200)
  confirm.setLayoutY(230)
  backBtn.setLayoutX(260)
  backBtn.setLayoutY(230)

  val userName = new TextField()
  val passWorld = new PasswordField()

//  val vBox = new VBox(30)
//  val hBox1 = new HBox(20)
//  val hBox2 = new HBox(20)
//  hBox1.getChildren.addAll(new Label("botId:"),userName)
//  hBox2.getChildren.addAll(new Label("botKey:"),passWorld)
//  vBox.getChildren.addAll(hBox1,hBox2,confirm)
//  vBox.setAlignment(Pos.CENTER)
//  vBox.setPadding(new Insets(100,50,100,160))

  val grid = new GridPane()
  grid.setHgap(10)
  grid.setVgap(10)
  grid.setPadding(new Insets(140,50,100,120))
  grid.add(new Label("botId:"), 0 ,0)
  grid.add(userName, 1 ,0)
  grid.add(new Label("botKey:"), 0 ,1)
  grid.add(passWorld, 1 ,1)
//  grid.add(confirm,1,5)
//  grid.setAlignment(Pos.BOTTOM_CENTER)

  group.getChildren.add(grid)
  group.getChildren.add(confirm)
  group.getChildren.add(backBtn)

  def getScene = this.scene

  var listener:BotSceneListener = _

  confirm.setOnAction(_ => listener.confirm(userName.getText(),passWorld.getText()))
  backBtn.setOnAction(_ => listener.comeBack())

  def setListener(listener: BotSceneListener):Unit = {
    this.listener = listener
  }
}
