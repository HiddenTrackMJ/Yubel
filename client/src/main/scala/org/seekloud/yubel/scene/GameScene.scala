package org.seekloud.yubel.scene

import org.seekloud.yubel.common.Constant.ColorsSetting
import org.seekloud.yubel.paperClient.Protocol.{Data4TotalSync, WinData}
import org.seekloud.yubel.paperClient._
import javafx.scene.canvas.Canvas
import javafx.scene.control.Button
import javafx.scene.{Group, Scene}
import javafx.scene.input.KeyCode
import javafx.scene.text.{Font, FontPosture, FontWeight, Text}
import javafx.stage.Screen

/**
  * Created by dry on 2018/10/29.
  **/

object GameScene{
  trait GameSceneListener {
    def onKeyPressed(e: KeyCode): Unit
  }
}

class GameScene(img: Int, frameRate: Int) {

  import GameScene._

  var gameSceneListener: GameSceneListener = _

  val screen= Screen.getPrimary.getVisualBounds
  println(s"----width--${screen.getMaxX.toInt}")
  println(s"----height--${screen.getMaxY.toInt}")
  protected val viewWidth = screen.getMaxX.toInt
  protected val viewHeight = screen.getMaxY.toInt
  val rankWidth = viewWidth
  val rankHeight = viewHeight/2
  val group = new Group()
//  val backgroundCanvas = new Canvas()
  val viewCanvas = new Canvas()
  val rankCanvas = new Canvas()
  val backBtn = new Button("退出房间")
  backBtn.setLayoutX(20)
  backBtn.setLayoutY(20)

  viewCanvas.setHeight(viewHeight)
  viewCanvas.setWidth(viewWidth)

  rankCanvas.setHeight(rankHeight)
  rankCanvas.setWidth(rankWidth)

  group.getChildren.add(viewCanvas)
  group.getChildren.add(rankCanvas)

  val view = new GameViewCanvas(viewCanvas, rankCanvas, img)
  val rank = new RankCanvas(rankCanvas)


  private val viewCtx = viewCanvas.getGraphicsContext2D

  val getScene: Scene = new Scene(group)

  def resetScreen(viewWidth: Int,viewHeight: Int,rankWidth: Int,rankHeight: Int): Unit = {
//    val viewWidth = 1200//1800
//    val viewHeight = 750//900
//    val rankWidth = 1200//1800
//    val rankHeight = 250//300

    view.resetScreen(viewWidth, viewHeight, rankWidth, rankHeight)
    rank.resetRankView(rankWidth, rankHeight)

//    viewCanvas.setWidth(viewWidth)
//    viewCanvas.setHeight(viewHeight)
//
//    rankCanvas.setWidth(rankWidth)
//    rankCanvas.setHeight(rankHeight)
  }

  def draw(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, championId: String): Unit = {
    view.drawGrid(uid, data, offsetTime, grid, championId, frameRate)
    view.drawSmallMap(data.snakes.filter(_.id == uid).map(_.header).head, data.snakes)
  }

  def drawGameWait(): Unit = {
    viewCtx.save()
    viewCtx.setFill(ColorsSetting.dieInfoBackgroundColor)
    viewCtx.fillRect(0, 0, viewWidth, viewHeight)
    viewCtx.setFill(ColorsSetting.dieInfoFontColor)
    viewCtx.setFont(Font.font(30))
    viewCtx.fillText("Please wait.", 150, 180)
    viewCtx.restore()
  }

  def drawGameOff(firstCome: Boolean): Unit = {
    rank.drawClearRank()
    viewCtx.save()
    viewCtx.setFill(ColorsSetting.dieInfoBackgroundColor)
    viewCtx.fillRect(0, 0, viewWidth, viewHeight)
    viewCtx.setFill(ColorsSetting.dieInfoFontColor)
    if (firstCome) {
      viewCtx.setFont(Font.font(30))
      viewCtx.fillText("Welcome.", 150, 180)
    } else {
      viewCtx.setFont(Font.font(30))
      viewCtx.fillText("Ops, connection lost.", 150, 180)
    }
    viewCtx.restore()
  }

  def drawGameWin(myId: String, winner: String, data: FrontProtocol.WinData4Draw,winningData:WinData): Unit = {
    rank.drawClearRank()
    view.drawGameWin(myId: String, winner: String, data, winningData)
  }

  def drawGameDie(killerOpt: Option[String], myScore: BaseScore, maxArea: Int): Unit = {
    rank.drawClearRank()
    view.drawGameDie(killerOpt, myScore,maxArea)
  }


  def drawRank(myId: String, snakes: List[SkDt], currentRank: List[Score],
               personalScore: Option[Score], personalRank: Option[Byte], currentNum: Byte): Unit = {
    rank.drawRank(myId, snakes, currentRank, personalScore, personalRank, currentNum)
  }

  def drawBarrage(killedName: String, killerName: String): Unit = {
    view.drawUserDieInfo(killedName,killerName)
  }


  def setGameSceneListener(listener: GameSceneListener) {
    gameSceneListener = listener
  }

}
