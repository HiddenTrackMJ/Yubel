package org.seekloud.yubel.scene

import javafx.collections.{FXCollections, ObservableList, ObservableMap}
import javafx.event.EventHandler
import javafx.geometry.Pos
import javafx.scene.control._
import javafx.scene.effect.DropShadow
import javafx.scene.image.{Image, ImageView}
import javafx.scene.input.MouseEvent
import javafx.scene.{Group, Scene}
import javafx.scene.layout.{BorderPane, HBox, Priority, VBox}

import scala.collection.mutable

abstract class RoomListSceneListener{
  def confirm(roomId: Int, mode: Int, hasPwd: Boolean)
  def gotoCreateRoomScene()
  def reFresh()
  def comeBack()
}

class RoomListScene {
  val width = 500
  val height = 500
  val shadow = new DropShadow()

  private val group = new Group()
  private val scene = new Scene(group, width, height)
  private val borderPane = new BorderPane()

  val hBox = new HBox(20)
  val vBox = new VBox(20)

  val roomLockMap:mutable.HashMap[Int, (Int, Boolean)] = mutable.HashMap.empty[Int, (Int, Boolean)]//(roomId -> (mode, hasPwd))
  var botList:List[String] = List.empty[String]
  private val observableList:ObservableList[String] = FXCollections.observableArrayList()
  private val listView = new ListView[String](observableList)
  private val confirmBtn = new Button("进入房间")
  confirmBtn.setStyle("-fx-font: 15 arial; -fx-base: #67B567; -fx-background-radius: 10px;")
  private val roomBtn = new Button("创建房间")
  roomBtn.setStyle("-fx-font: 15 arial; -fx-base: #307CF0; -fx-background-radius: 10px;")
  private val refreshBtn = new Button("刷新列表")
  refreshBtn.setStyle("-fx-font: 15 arial; -fx-base: #E6AF5F; -fx-background-radius: 10px;")
  private val backBtn = new Button("返回")
  backBtn.setStyle("-fx-font: 15 arial; -fx-base: #CA5C54; -fx-background-radius: 10px;")

  confirmBtn.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      confirmBtn.setEffect(shadow)
    }
  })

  confirmBtn.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      confirmBtn.setEffect(null)
    }
  })

  roomBtn.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      roomBtn.setEffect(shadow)
    }
  })

  roomBtn.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      roomBtn.setEffect(null)
    }
  })

  refreshBtn.addEventHandler(MouseEvent.MOUSE_ENTERED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      refreshBtn.setEffect(shadow)
    }
  })

  refreshBtn.addEventHandler(MouseEvent.MOUSE_EXITED,new EventHandler[MouseEvent] {
    override def handle(event: MouseEvent): Unit = {
      refreshBtn.setEffect(null)
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

  var listener: RoomListSceneListener = _

//  confirmBtn.setPrefSize(100,20)
  vBox.getChildren.addAll(confirmBtn,roomBtn,refreshBtn,backBtn)
  vBox.setAlignment(Pos.CENTER)
  hBox.getChildren.addAll(listView, vBox)
  hBox.setAlignment(Pos.CENTER)
//  HBox.setHgrow(listView,Priority.ALWAYS)
  //  borderPane.prefHeightProperty().bind(scene.heightProperty())
  borderPane.prefWidthProperty().bind(scene.widthProperty())
  borderPane.setCenter(hBox)
  group.getChildren.add(borderPane)

  def updateRoomList(roomList:List[String]):Unit = {
    println(s"updateRoomList: $roomList")
    this.botList = roomList
    this.roomLockMap.clear()
    observableList.clear()
    roomList.sortBy(t => t).map { s =>
      val roomInfos = s.split("-")
      val roomId = roomInfos(0).toInt
      val modeName = if(roomInfos(1)=="0") "正常" else if(roomInfos(2)=="1") "反转" else "加速"
      val hasPwd = if(roomInfos(2)=="true") true else false
      roomLockMap += roomId -> (roomInfos(1).toInt, hasPwd)
      roomId.toString + "-" + modeName
    } foreach observableList.add
  }

  listView.setCellFactory(_ => new ListCell[String](){
    val img = new ImageView("img/suo.png")
    val img1 = new ImageView("img/suo_1.png")
    img.setFitWidth(15)
    img.setFitHeight(15)
    img1.setFitWidth(15)
    img1.setFitHeight(15)

    override def updateItem(item: String, empty: Boolean): Unit = {
      super.updateItem(item, empty)
      if(empty) {
        setText(null)
        setGraphic(null)
      } else {
//        println(s"item: $item")
        val roomId = item.split("-")(0).toInt
        if(roomLockMap.contains(roomId) && roomLockMap(roomId)._2){
          setGraphic(img1)
        }
        else{
          setGraphic(img)
        }
        setText(item)
      }
    }
  })

  def getScene = this.scene

  confirmBtn.setOnAction{_ =>
    val selectedInfo = listView.getSelectionModel.selectedItemProperty().get()
    val roomId = selectedInfo.split("-").head.toInt
    listener.confirm(roomId, roomLockMap(roomId)._1, roomLockMap(roomId)._2)
  }
  roomBtn.setOnAction(_ => listener.gotoCreateRoomScene())
  refreshBtn.setOnAction(_ => listener.reFresh())
  backBtn.setOnAction(_ => listener.comeBack())
}
