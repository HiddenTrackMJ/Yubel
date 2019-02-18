package org.seekloud.yubel.scene

import javafx.scene.{Group, Scene}
import javafx.scene.canvas.Canvas
import javafx.scene.control.{Button, PasswordField, RadioButton, ToggleGroup}
import javafx.scene.image.Image
import javafx.scene.paint.Color
import javafx.scene.text.Font

abstract class CreateRoomSceneListener {
  def createRoom(mode: Int, img: Int, pwd: String)
}

class CreateRoomScene {

  var selectedMode: Int = 0
  var selectedImg: Int = 0

  val width = 500
  val height = 500
  val group = new Group
  var listener: CreateRoomSceneListener = _

  val canvas = new Canvas(width, height)
  val canvasCtx = canvas.getGraphicsContext2D
  canvasCtx.setFill(Color.rgb(51, 51, 51))
  canvasCtx.fillRect(0, 0, width, height)

  val modeImg0 = new Image("img/coffee2.png")
  val modeImg1 = new Image("img/game.png")
  val modeImg2 = new Image("img/rocket1.png")

  val headerImg0 = new Image("img/luffy.png")
  val headerImg1 = new Image("img/fatTiger.png")
  val headerImg2 = new Image("img/Bob.png")
  val headerImg3 = new Image("img/yang.png")
  val headerImg4 = new Image("img/smile.png")
  val headerImg5 = new Image("img/pig.png")

  val toggleGroup = new ToggleGroup()
  val mode0 = new RadioButton("")
  mode0.setSelected(true)
  mode0.setToggleGroup(toggleGroup)
  mode0.setUserData(0)
  val mode1 = new RadioButton()
  mode1.setToggleGroup(toggleGroup)
  mode1.setUserData(1)
  val mode2 = new RadioButton()
  mode2.setToggleGroup(toggleGroup)
  mode2.setUserData(2)

  canvasCtx.setFill(Color.WHITE)
  canvasCtx.setFont(Font.font(15))
  canvasCtx.fillText("正常模式", 80, 170)
  canvasCtx.fillText("反转模式", 230, 170)
  canvasCtx.fillText("加速模式", 380, 170)
  canvasCtx.setFont(Font.font(18))
  canvasCtx.fillText("选择头像：", 210, 240)

  mode0.setLayoutX(100)
  mode0.setLayoutY(180)
  mode1.setLayoutX(250)
  mode1.setLayoutY(180)
  mode2.setLayoutX(400)
  mode2.setLayoutY(180)

  val toggleGroup2 = new ToggleGroup()
  val img0 = new RadioButton("")
  img0.setSelected(true)
  img0.setToggleGroup(toggleGroup2)
  img0.setUserData(0)
  val img1 = new RadioButton("")
  img1.setToggleGroup(toggleGroup2)
  img1.setUserData(1)
  val img2 = new RadioButton("")
  img2.setToggleGroup(toggleGroup2)
  img2.setUserData(2)
  val img3 = new RadioButton("")
  img3.setToggleGroup(toggleGroup2)
  img3.setUserData(3)
  val img4 = new RadioButton("")
  img4.setToggleGroup(toggleGroup2)
  img4.setUserData(4)
  val img5 = new RadioButton("")
  img5.setToggleGroup(toggleGroup2)
  img5.setUserData(5)

  img0.setLayoutX(110)
  img0.setLayoutY(310)
  img1.setLayoutX(160)
  img1.setLayoutY(310)
  img2.setLayoutX(210)
  img2.setLayoutY(310)
  img3.setLayoutX(260)
  img3.setLayoutY(310)
  img4.setLayoutX(310)
  img4.setLayoutY(310)
  img5.setLayoutX(360)
  img5.setLayoutY(310)

  canvasCtx.drawImage(modeImg0, 50, 30, 120, 120)
  canvasCtx.drawImage(modeImg1, 190, 30, 120, 120)
  canvasCtx.drawImage(modeImg2, 340, 40, 120, 110)

  canvasCtx.drawImage(headerImg0, 100, 250, 40, 40)
  canvasCtx.drawImage(headerImg1, 150, 250, 40, 40)
  canvasCtx.drawImage(headerImg2, 200, 250, 40, 40)
  canvasCtx.drawImage(headerImg3, 250, 250, 40, 40)
  canvasCtx.drawImage(headerImg4, 300, 250, 40, 40)
  canvasCtx.drawImage(headerImg5, 350, 250, 40, 40)

  val pwdField = new PasswordField()
  pwdField.setPromptText("房间密码")
  pwdField.setPrefWidth(80)
  pwdField.setLayoutX(180)
  pwdField.setLayoutY(400)

  val roomBtn = new  Button("创建房间")
  roomBtn.setStyle("-fx-font: 15 arial; -fx-base: #67B567; -fx-background-radius: 10px;")

  roomBtn.setLayoutX(280)
  roomBtn.setLayoutY(400)

  group.getChildren.add(canvas)
  group.getChildren.add(mode0)
  group.getChildren.add(mode1)
  group.getChildren.add(mode2)
  group.getChildren.add(img0)
  group.getChildren.add(img1)
  group.getChildren.add(img2)
  group.getChildren.add(img3)
  group.getChildren.add(img4)
  group.getChildren.add(img5)
  group.getChildren.add(pwdField)
  group.getChildren.add(roomBtn)
  private val scene = new Scene(group)

  def getScene = scene

  roomBtn.setOnAction(_ => listener.createRoom(selectedMode, selectedImg, pwdField.getText))

  toggleGroup.selectedToggleProperty().addListener(_ => selectMode())
  toggleGroup2.selectedToggleProperty().addListener(_ => selectImg())

  def selectMode(): Unit = {
    val rst = toggleGroup.getSelectedToggle.getUserData.toString.toInt
    selectedMode = rst

  }

  def selectImg(): Unit ={
    val rst = toggleGroup2.getSelectedToggle.getUserData.toString.toInt
    selectedImg = rst
  }

  def setListener(listener: CreateRoomSceneListener): Unit ={
    this.listener = listener
  }
}
