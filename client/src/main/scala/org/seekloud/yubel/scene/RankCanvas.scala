package org.seekloud.yubel.scene

import org.seekloud.yubel.common.Constant
import org.seekloud.yubel.common.Constant.ColorsSetting
import org.seekloud.yubel.paperClient.{BorderSize, Point, Sc, SkDt}
import javafx.geometry.VPos
import javafx.scene.paint.Color
import javafx.scene.text.{Font, FontPosture, FontWeight, Text}
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.Image
/**
  * Created by dry on 2018/10/29.
  **/
class RankCanvas(canvas: Canvas)  {

  private var realWindowWidth = canvas.getWidth
  private var realWindowHeight = canvas.getHeight
  private var windowBoundary = Point(realWindowWidth.toFloat, realWindowHeight.toFloat)
  private val ctx = canvas.getGraphicsContext2D
  private val goldImg = new Image("gold.png")
  private val silverImg = new Image("silver.png")
  private val bronzeImg = new Image("bronze.png")
  private val killImg = new Image("kill.png")
  private val border = Point(BorderSize.w, BorderSize.h)
  private val canvasSize = (border.x - 1) * (border.y - 1)

  private val textLineHeight = 15
  private val fillWidth = 33
  private var lastRankNum = 0 //清屏用
  private val myRankBaseLine = 4

//  def getRankView: Canvas = canvas

  def resetRankView(rankWidth:Int, rankHeight:Int): Unit = {
    canvas.setWidth(rankWidth)
    canvas.setHeight(rankHeight)
    realWindowWidth = canvas.getWidth
    realWindowHeight = canvas.getHeight
    windowBoundary = Point(realWindowWidth.toFloat, realWindowHeight.toFloat)
  }

  def drawRank(uid: String, snakes: List[SkDt], currentRank: List[Sc],
               personalScoreOp: Option[Sc], personalRankOp: Option[Byte], currentNum: Byte): Unit = {
    val personalScore = if (personalScoreOp.isDefined) personalScoreOp.get else currentRank.filter(_.id == uid).head
    val personalRank = if (personalRankOp.isDefined) personalRankOp.get else currentRank.indexOf(personalScore) + 1

    val leftBegin = 20
    val rightBegin = windowBoundary.x - 230

    drawClearRank()//绘制前清除canvas

    lastRankNum = currentRank.length

    ctx.setGlobalAlpha(1.0)
    ctx.setTextBaseline(VPos.TOP)

    val mySnake = snakes.filter(_.id == uid).head
    val baseLine = 2
    ctx.setFont(Font.font(22))
    ctx.setFill(Color.rgb(0,0,0))
    drawTextLine(s"KILL: ", leftBegin, 0, baseLine)
    ctx.drawImage(killImg, leftBegin + 55, textLineHeight, textLineHeight * 1.4, textLineHeight * 1.4)
    drawTextLine(s" x ${mySnake.kill}", leftBegin + 55 + (textLineHeight * 1.4).toInt, 0, baseLine)

    if (personalScore.id == uid) {
      val color = snakes.find(_.id == uid).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
      ctx.setGlobalAlpha(0.6)
      ctx.setFill(color)
      ctx.save()
      ctx.fillRect(leftBegin, (myRankBaseLine - 1) * textLineHeight, fillWidth + windowBoundary.x / 8 * (personalScore.area.toDouble / canvasSize), textLineHeight + 10)
      ctx.restore()

      ctx.setGlobalAlpha(1)
      ctx.setFont(Font.font(22))
      ctx.setFill(Color.rgb(0,0,0))
      drawTextLine(f"${personalScore.area.toDouble / canvasSize * 100}%.2f" + s"%", leftBegin, 0, myRankBaseLine)
    }

    val currentRankBaseLine = 2
    var index = 0
    ctx.setFont(Font.font(14))

    drawTextLine(s" --- Current Rank ---   players:$currentNum", rightBegin.toInt, index, currentRankBaseLine)
    if (currentRank.lengthCompare(3) >= 0) {
      ctx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
      ctx.drawImage(silverImg, rightBegin - 5 - textLineHeight, textLineHeight * 3, textLineHeight, textLineHeight)
      ctx.drawImage(bronzeImg, rightBegin - 5 - textLineHeight, textLineHeight * 4, textLineHeight, textLineHeight)
    }
    else if (currentRank.lengthCompare(2) == 0) {
      ctx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
      ctx.drawImage(silverImg, rightBegin - 5 - textLineHeight, textLineHeight * 3, textLineHeight, textLineHeight)
    }
    else {
      ctx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
    }
    currentRank.foreach { score =>
      val color = snakes.find(_.id == score.id).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
      ctx.setGlobalAlpha(0.6)
      ctx.setFill(color)
      ctx.save()
      ctx.fillRect(windowBoundary.x - 20 - fillWidth - windowBoundary.x / 8 * (score.area.toDouble / canvasSize), (index + currentRankBaseLine) * textLineHeight,
        fillWidth + windowBoundary.x / 8 * (score.area.toDouble / canvasSize), textLineHeight)
      ctx.restore()

      ctx.setGlobalAlpha(1)
      ctx.setFill(Color.rgb(0,0,0))
      index += 1
      drawTextLine(s"[$index]: ${score.n.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
      drawTextLine(s"area=" + f"${score.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 70, index, currentRankBaseLine)
      drawTextLine(s"kill=${score.k}", rightBegin.toInt + 160, index, currentRankBaseLine)
    }

    index += 1
    val color = snakes.find(_.id == personalScore.id).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
    ctx.setGlobalAlpha(0.6)
    ctx.setFill(color)
    ctx.save()
    ctx.fillRect(windowBoundary.x - 20 - fillWidth - windowBoundary.x / 8 * (personalScore.area.toDouble / canvasSize), (index + currentRankBaseLine) * textLineHeight,
      fillWidth + windowBoundary.x / 8 * (personalScore.area.toDouble / canvasSize), textLineHeight)
    ctx.restore()

    ctx.setGlobalAlpha(1)
    ctx.setFill(Color.rgb(0,0,0))

    index += 1
    drawTextLine(s"[$personalRank]: ${personalScore.n.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
    drawTextLine(s"area=" + f"${personalScore.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 70, index, currentRankBaseLine)
    drawTextLine(s"kill=${personalScore.k}", rightBegin.toInt + 160, index, currentRankBaseLine)
  }

  def drawClearRank(): Unit = {
    val width = canvas.getWidth
    val height = canvas.getHeight
    ctx.clearRect(0, 0, width, height)
//    val rightBegin = windowBoundary.x - 230//230
//    ctx.clearRect(0, textLineHeight, fillWidth + windowBoundary.x / 6, textLineHeight * 4) //绘制前清除canvas
//    ctx.clearRect(rightBegin - 10 - textLineHeight, textLineHeight, 210 + 10 + textLineHeight, textLineHeight * (lastRankNum + 1) + 3)
//    ctx.clearRect(20, textLineHeight * 5, 700, textLineHeight * 2)//玩家死亡清除fps
  }

  def drawTextLine(str: String, x: Int, lineNum: Int, lineBegin: Int = 0): Unit = {
    ctx.fillText(str, x, (lineNum + lineBegin - 1) * textLineHeight)
  }

  def drawTextLineSelf(str: String, x: Int, lineNum: Int, lineBegin: Int = 0): Unit = {
    ctx.fillText(str, x, (lineNum + lineBegin) * textLineHeight + canvas.getHeight/15)
  }


}
