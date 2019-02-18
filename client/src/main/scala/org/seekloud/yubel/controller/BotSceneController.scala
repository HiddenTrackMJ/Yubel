package org.seekloud.yubel.controller

import org.seekloud.yubel.Boot
import org.seekloud.yubel.common.{AppSetting, Context}
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.yubel.scene.{BotScene, BotSceneListener, LayeredGameScene, ModeScene}
import org.seekloud.yubel.utils.Api4GameAgent.{botKey2Token, linkGameAgent}
import org.seekloud.yubel.Boot.executor
import org.slf4j.LoggerFactory

class BotSceneController(modeScene: ModeScene,botScene: BotScene, context: Context) {
  private[this] val log = LoggerFactory.getLogger(this.getClass)

  botScene.setListener(new BotSceneListener {
    override def confirm(botId: String, botKey: String): Unit = {
      botKey2Token(botId, botKey).map {
        case Right(data) =>
          val gameId = AppSetting.esheepGameId
          linkGameAgent(gameId, "bot" + botId, data.token).map {
            //todo 连接webSocket
            case Right(rst) =>
              Boot.addToPlatform {
                val domain = rst.gsPrimaryInfo.domain
                val layeredGameScreen = new LayeredGameScene(0, 150)
                context.switchScene4Play(layeredGameScreen.getScene, "layered", false, false,1630,660)
                layeredGameScreen.drawGameWait(context.getStage.getWidth.toInt, context.getStage.getHeight.toInt)
                new BotController(PlayerInfoInClient(botId, botKey, rst.accessCode), context, layeredGameScreen, domain)
              }
            case Left(e) =>
              log.error(s"bot link game agent error, $e")
          }

        case Left(e) =>
          log.error(s"botKey2Token error, $e")
      }
    }

    override def comeBack(): Unit = {
      Boot.addToPlatform(
        context.switchScene(modeScene.getScene, "模式选择", false)
      )
    }
  })

  def showScene() {
    Boot.addToPlatform {
      context.switchScene(botScene.getScene, "Bot模式", false)
    }
  }
}
