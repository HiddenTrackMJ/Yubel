package org.seekloud.yubel.scene

/**
  * Created by dry on 2018/12/5.
  */

import org.seekloud.yubel.common.Constant.{ColorsSetting,_}
import org.seekloud.yubel.paperClient.Protocol.{Data4TotalSync, WinData}
import org.seekloud.yubel.paperClient._
import javafx.scene.canvas.Canvas
import javafx.scene.{Group, Scene}
import javafx.scene.input.KeyCode
import javafx.scene.media.AudioClip
import javafx.scene.text.{Font, FontPosture, FontWeight, Text}
import javafx.stage.Screen
import javafx.scene.paint.Color


class LayeredGameScene (img: Int, frameRate: Int) {

  import GameScene._


  val screen= Screen.getPrimary.getVisualBounds
  println(s"----width--${screen.getMaxX.toInt}")
  println(s"----height--${screen.getMaxY.toInt}")
  protected val viewWidth = layeredCanvasWidth
  protected val viewHeight = layeredCanvasHeight
  private val humanViewWidth = CanvasWidth
  private val humanViewHeight = CanvasHeight
  val group = new Group()

  val positionCanvas = new Canvas()
  val viewCanvas = new Canvas()
  val rankCanvas = new Canvas()
  val BorderCanvas = new Canvas()
  val selfViewCanvas = new Canvas()
  val selfCanvas = new Canvas()
  val humanViewCanvas = new Canvas()
  val allCanvas = new Canvas()
  val headerCanvas = new Canvas()

  allCanvas.setHeight(0)
  allCanvas.setWidth(0)

  positionCanvas.setHeight(viewHeight)
  positionCanvas.setWidth(viewWidth)
  positionCanvas.setLayoutY(10)
  positionCanvas.setLayoutX(815)
  positionCanvas.setId("0")

  BorderCanvas.setHeight(viewHeight)
  BorderCanvas.setWidth(viewWidth)
  BorderCanvas.setLayoutY(10)
  BorderCanvas.setLayoutX(1220)
  BorderCanvas.setId("1")

  viewCanvas.setHeight(viewHeight)
  viewCanvas.setWidth(viewWidth)
  viewCanvas.setLayoutY(215)
  viewCanvas.setLayoutX(815)
  viewCanvas.setId("2")

  headerCanvas.setHeight(viewHeight)
  headerCanvas.setWidth(viewWidth)
  headerCanvas.setLayoutY(215)
  headerCanvas.setLayoutX(1220)
  headerCanvas.setId("3")

  selfViewCanvas.setHeight(viewHeight)
  selfViewCanvas.setWidth(viewWidth)
  selfViewCanvas.setLayoutY(420)
  selfViewCanvas.setLayoutX(815)
  selfViewCanvas.setId("4")

  selfCanvas.setHeight(viewHeight)
  selfCanvas.setWidth(viewWidth)
  selfCanvas.setLayoutY(420)
  selfCanvas.setLayoutX(1220)
  selfCanvas.setId("5")

  rankCanvas.setHeight(viewHeight)
  rankCanvas.setWidth(viewWidth)
  rankCanvas.setLayoutY(415)
  rankCanvas.setLayoutX(410)
  rankCanvas.setId("6")

  humanViewCanvas.setHeight(humanViewHeight + 210)
  humanViewCanvas.setWidth(humanViewWidth)
  humanViewCanvas.setLayoutY(10)
  humanViewCanvas.setLayoutX(10)
  humanViewCanvas.setId("7")


  
  

  group.getChildren.add(viewCanvas)
  group.getChildren.add(rankCanvas)
  group.getChildren.add(positionCanvas)
  group.getChildren.add(BorderCanvas)
  group.getChildren.add(selfViewCanvas)
  group.getChildren.add(selfCanvas)
  group.getChildren.add(humanViewCanvas)
  group.getChildren.add(allCanvas)
  group.getChildren.add(headerCanvas)

  val rank = new RankCanvas(rankCanvas)
  val layered = new LayeredCanvas(viewCanvas,rankCanvas,positionCanvas,BorderCanvas,selfViewCanvas,selfCanvas,humanViewCanvas,headerCanvas,img)


  private val selfViewCtx = selfViewCanvas.getGraphicsContext2D

  private val allCtx = allCanvas.getGraphicsContext2D

  val getScene: Scene = new Scene(group)


  def draw(currentRank:List[Sc], uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long,
           grid: Grid, championId: String, myActions: Map[Int,Int],
           personalScore: Option[Sc], personalRank: Option[Byte], currentNum: Byte): Unit = {
    layered.drawPosition(data.snakes.filter(_.id == uid).map(_.header).head,data.snakes.find(_.id == championId).map(_.header),uid == championId)
    layered.drawBorder(uid, data, offsetTime, grid, frameRate)
    layered.drawSelfView(uid, data, offsetTime, grid,  frameRate, championId)
    layered.drawSelf(uid, data, offsetTime, grid, frameRate, championId)
//    layered.drawRank(uid, data.snakes, currentRank)
    layered.drawBody(uid, data, offsetTime, grid, frameRate, championId)
    layered.drawHumanView(currentRank,uid, data, offsetTime, grid, frameRate, myActions, championId,personalScore, personalRank, currentNum)
    layered.drawHumanMap(data.snakes.filter(_.id == uid).map(_.header).head, data.snakes, championId)
    layered.drawHeader(uid, data, offsetTime, grid, frameRate, championId)
  }


  def drawGameWait(w:Int,h:Int): Unit = {
    println(w,h)
    allCanvas.setHeight(h)
    allCanvas.setWidth(w)
    allCtx.save()
    allCtx.setFill(ColorsSetting.dieInfoBackgroundColor)
    allCtx.fillRect(0, 0, w, h)
    allCtx.setFill(ColorsSetting.dieInfoFontColor)
    allCtx.setFont(Font.font(30))
    allCtx.fillText("Waiting for bots to join ...", 150, 180)
    allCtx.restore()
  }

  def cleanGameWait(w:Int,h:Int): Unit = {
    allCanvas.setHeight(1080)
    allCanvas.setWidth(1920)
    allCtx.clearRect(0, 0, 1920, 1080)
    allCtx.setFill(Color.BLACK)
    allCtx.save()
//    allCtx.clearRect(0, 0, 1920, 1080)
//    allCtx.setFill(ColorsSetting.dieInfoFontColor)
//    allCtx.setFont(Font.font(30))
//    allCtx.fillRect(0,0,1920,10)
//    allCtx.fillRect(0,10,10,620)
//    allCtx.fillRect(1620,10,10,620)
//    allCtx.fillRect(0,650,1920,10)
    allCtx.restore()
  }

  def drawGameOff(firstCome: Boolean): Unit = {
    rank.drawClearRank()
    selfViewCtx.save()
    selfViewCtx.setFill(ColorsSetting.dieInfoBackgroundColor)
    selfViewCtx.fillRect(0, 0, viewWidth, viewHeight)
    selfViewCtx.setFill(ColorsSetting.dieInfoFontColor)
    if (firstCome) {
      selfViewCtx.setFont(Font.font(30))
      selfViewCtx.fillText("Welcome.", 150, 180)
    } else {
      selfViewCtx.setFont(Font.font(30))
      selfViewCtx.fillText("Ops, connection lost.", 150, 180)
    }
    selfViewCtx.restore()
  }

//  def drawGameWin(myId: String, winner: String, data: Data4TotalSync,winningData:WinData): Unit = {
//    rank.drawClearRank()
//    layered.drawGameWin(myId: String, winner: String, data: Data4TotalSync,winningData:WinData)
//  }

//  def drawGameDie(killerOpt: Option[String], myScore: BaseScore, maxArea: Int): Unit = {
//    rank.drawClearRank()
//    layered.drawGameDie(killerOpt, myScore,maxArea)
//  }


  def drawRank(myId: String, snakes: List[SkDt], currentRank: List[Sc]): Unit = {
    layered.drawRank(myId, snakes, currentRank)
  }


//  def drawBarrage(killedName: String, killerName: String): Unit = {
//    layered.drawUserDieInfo(killedName,killerName)
//  }

}
