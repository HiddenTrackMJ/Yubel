package org.seekloud.yubel.common

import javafx.scene.Scene
import javafx.stage.Stage

/**
  * Created by dry on 2018/10/29.
  **/
class Context(stage: Stage) {

  def getStage: Stage = stage
  def switchScene(scene: Scene, title:String = "carnie", fullScreen: Boolean,resizable: Boolean = false) = {
    stage.setScene(scene)
    stage.sizeToScene()
    stage.setResizable(resizable)
    stage.setTitle(title)
//    stage.setWidth(width)
//    stage.setHeight(height)
//    stage.setIconified(fullScreen)
    stage.setFullScreen(fullScreen)
//    stage.setMaximized(fullScreen)
    stage.show()
//    stage.fullScreenProperty()
  }

  def switchScene4Play(scene: Scene, title:String = "carnie", fullScreen: Boolean,resizable: Boolean = false,width: Int = 1920,height: Int = 1080) = {
    stage.setScene(scene)
    stage.sizeToScene()
    stage.setResizable(resizable)
    stage.setTitle(title)
    stage.setWidth(width)
    stage.setHeight(height)
    stage.setX(130)
    stage.setY(185)
    //    stage.setIconified(fullScreen)
    stage.setFullScreen(fullScreen)
    //    stage.setMaximized(fullScreen)
    stage.show()
    //    stage.fullScreenProperty()
  }

}
