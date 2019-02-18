package org.seekloud.yubel.controller

import org.seekloud.yubel.Boot
import org.seekloud.yubel.Boot.executor
import org.seekloud.yubel.scene._
import org.seekloud.yubel.common.{AppSetting, Context}
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.yubel.paperClient.Protocol.{frameRate1, frameRate2}
import org.seekloud.yubel.utils.Api4GameAgent.linkGameAgent
import javafx.collections.FXCollections
import javafx.scene.control.ButtonBar.ButtonData
import javafx.scene.control._
import javafx.scene.layout.GridPane
import org.slf4j.LoggerFactory


class SelectController(playerInfoInClient: PlayerInfoInClient, selectScene: SelectScene, context: Context) {
  private[this] val log = LoggerFactory.getLogger(this.getClass)

  selectScene.setListener(new SelectSceneListener{
    override def joinGame(mode: Int, img: Int): Unit = {
      val frameRate = if(mode==2) frameRate2 else frameRate1
      val playGameScreen = new GameScene(img, frameRate)
//        val LayeredGameScreen = new LayeredGameScene(img, frameRate)
//        val x = false
//        if(x) {
      val gameId = AppSetting.esheepGameId
      Boot.addToPlatform(
        linkGameAgent(gameId, playerInfoInClient.id, playerInfoInClient.token).map {
          case Right(r) =>
            Boot.addToPlatform {
              context.switchScene(playGameScreen.getScene, fullScreen = true, resizable = true)
              new GameController(playerInfoInClient.copy(accessCode = r.accessCode), context, playGameScreen, mode, frameRate).start(r.gsPrimaryInfo.domain, mode, img)
            }

          case Left(e) =>
            log.debug(s"linkGameAgent..$e")
        }
      )

//        }
//        else {
//          context.switchScene(LayeredGameScreen.getScene,fullScreen = true)
//          new GameController(playerInfoInClient, context, playGameScreen, mode, frameRate).start(domain, mode, img)
//        }

    }

    override def gotoRoomList(): Unit = {
      Boot.addToPlatform {
        val roomListScene = new RoomListScene()
        new RoomListController(playerInfoInClient, selectScene, roomListScene, context).showScene
      }
    }
  })

  //todo 创建房间的弹窗demo
  def initDialog = {
    val dialog = new Dialog[(String,String,String)]()
    dialog.setTitle("test")
    val a = new ChoiceBox[String](FXCollections.observableArrayList("正常","反转","加速"))
    a.setValue("反转")
    val tF = new TextField()
    val loginButton = new ButtonType("确认", ButtonData.OK_DONE)
    val grid = new GridPane
    grid.add(a, 0 ,0)
    grid.add(tF, 0 ,1)
    dialog.getDialogPane.getButtonTypes.addAll(loginButton, ButtonType.CANCEL)
    dialog.getDialogPane.setContent(grid)
    dialog.setResultConverter(dialogButton =>
      if(dialogButton == loginButton)
        (a.getValue, "a", tF.getText)
      else
        null
    )
    val rst = dialog.showAndWait()
    rst.ifPresent(a =>
      println(s"${a._1}-${a._3}")
    )
  }

  def showScene: Unit = {
    Boot.addToPlatform {
      context.switchScene(selectScene.getScene, "选择游戏模式及头像", false)
    }
  }
}
