package org.seekloud.yubel.scene

import org.seekloud.yubel.ptcl.EsheepPtcl.BotInfo
import javafx.collections.{FXCollections, ObservableList}
import javafx.geometry.Pos
import javafx.scene.{Group, Scene}
import javafx.scene.control._
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{BorderPane, HBox}

abstract class BotListSceneListener {
  def joinGame(mode: Int, img: Int)
}

class BotListScene {
  val width = 500
  val height = 500

  private val group = new Group()
  private val scene = new Scene(group, width, height)
  private val borderPane = new BorderPane()

  val hBox = new HBox(20)

//  var botList:List[String] = List.empty[String]
  private val observableList:ObservableList[String] = FXCollections.observableArrayList()
  private val listView = new ListView[String](observableList)
  private val confirmBtn = new Button("加入游戏")

  var listener: BotListSceneListener = _

  //  confirmBtn.setPrefSize(100,20)
  hBox.getChildren.addAll(listView, confirmBtn)
  hBox.setAlignment(Pos.CENTER)
  //  HBox.setHgrow(listView,Priority.ALWAYS)
  //  borderPane.prefHeightProperty().bind(scene.heightProperty())
  borderPane.prefWidthProperty().bind(scene.widthProperty())
  borderPane.setCenter(hBox)
  group.getChildren.add(borderPane)

  def updateBotList(botList:List[BotInfo]):Unit = {
//    this.botList = botList
    observableList.clear()
    botList.map { s => //todo
      s.botKey
    } foreach observableList.add
    //    println(observableList)
  }

//  listView.setCellFactory(_ => new ListCell[String](){
//  val img = new ImageView("img/Bob.png")
//    val img1 = new ImageView("img/luffy.png")
//    img.setFitWidth(15)
//    img.setFitHeight(15)
//    img1.setFitWidth(15)
//    img1.setFitHeight(15)
//
//    override def updateItem(item: String, empty: Boolean): Unit = {
//      super.updateItem(item, empty)
//      if(empty) {
//        setText(null)
//        setGraphic(null)
//      } else {
//        //        println(s"item: $item")
//        val roomId = item.split("-")(0).toInt
//        if(roomLockMap.contains(roomId) && roomLockMap(roomId)._2){
//          setGraphic(img)
//        }
//        else{
//          setGraphic(img1)
//        }
//        setText(item)
//      }
//    }
//  })

  def getScene = this.scene

//  confirmBtn.setOnAction{_ =>
//    val selectedInfo = listView.getSelectionModel.selectedItemProperty().get()
//    val roomId = selectedInfo.split("-").head.toInt
//    listener.confirm(roomId, roomLockMap(roomId)._1, roomLockMap(roomId)._2)
//  }
    confirmBtn.setOnAction(_ => println(listView.getSelectionModel.selectedItemProperty().get()))
}