package org.seekloud.yubel.controller

import org.seekloud.yubel.Boot
import org.seekloud.yubel.common.Context
import org.seekloud.yubel.scene.{BotScene, LoginScene, ModeScene, ModeSceneListener}

class ModeController(modeScene: ModeScene, context: Context) {

  modeScene.setListener(new ModeSceneListener {
    override def gotoNormalScene(): Unit = {
      Boot.addToPlatform{
        val loginScene = new LoginScene
        val loginController = new LoginController(modeScene, loginScene, context)
        loginController.init()
        loginController.showScene()
      }
    }

    override def gotoBotScene(): Unit = {
      Boot.addToPlatform{
        val botScene = new BotScene
        val botController = new BotSceneController(modeScene,botScene,context)
        botController.showScene()
      }
    }
  })

  def showScene() {
    Boot.addToPlatform {
      context.switchScene(modeScene.getScene, "模式选择", false)
    }
  }
}
