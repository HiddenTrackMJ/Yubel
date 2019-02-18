package org.seekloud.yubel.controller

import org.seekloud.yubel.Boot
import org.seekloud.yubel.Boot.executor
import org.seekloud.yubel.common.{AppSetting, Context}
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.yubel.paperClient.Protocol.{frameRate1, frameRate2}
import org.seekloud.yubel.scene._
import org.seekloud.yubel.utils.Api4GameAgent.linkGameAgent
import org.slf4j.LoggerFactory

class CreateRoomController(playerInfoInClient: PlayerInfoInClient, createRoomScene: CreateRoomScene, context: Context) {
  private val log = LoggerFactory.getLogger(this.getClass)
//  private val domain = AppSetting.esheepDomain

  createRoomScene.setListener(new CreateRoomSceneListener {
    override def createRoom(mode: Int, img: Int, pwd: String): Unit = {
      Boot.addToPlatform {
        val frameRate = if(mode==2) frameRate2 else frameRate1
//        println(s"pwd: $pwd")
        val gameId = AppSetting.esheepGameId
        linkGameAgent(gameId, playerInfoInClient.id, playerInfoInClient.token).map {
          case Right(r) =>
            val playGameScreen = new GameScene(img, frameRate)
            Boot.addToPlatform{
              context.switchScene(playGameScreen.getScene, fullScreen = true, resizable = true)
              new GameController(playerInfoInClient.copy(accessCode=r.accessCode), context, playGameScreen, mode, frameRate).createRoom(r.gsPrimaryInfo.domain, mode, img, pwd)
            }

          case Left(e) =>
            log.debug(s"linkGameAgent..$e")
        }
      }
    }
  })

  def showScene: Unit = {
    Boot.addToPlatform {
      context.switchScene(createRoomScene.getScene, "创建房间", false)
    }
  }
}
