package org.seekloud.yubel.paperClient

import org.seekloud.yubel.common.Constant.ColorsSetting
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.yubel.util.TimeTool
import javafx.scene.paint.Color
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.{Button, Canvas, Image}

/**
  * Created by dry on 2018/9/3.
  **/
class DrawGame(
  ctx: CanvasRenderingContext2D,
  canvas: Canvas,
  img: Int = 0
) {

  private var windowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
  private val border = Point(BorderSize.w, BorderSize.h)
  private val window = Point(Window.w, Window.h)
  private var canvasUnit = (dom.window.innerWidth.toInt / window.x).toInt
  private val canvasSize = (border.x - 1) * (border.y - 1)
  var fieldScale = 1.0

  private val textLineHeight = 15
  private val fillWidth = 33

  private[this] val rankCanvas = dom.document.getElementById("RankView").asInstanceOf[Canvas] //排行榜canvas
  private[this] val rankCtx = rankCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  private[this] val borderCanvas = dom.document.getElementById("BorderView").asInstanceOf[Canvas] //边界canvas
  private[this] val borderCtx = borderCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private val bodyAttribute = dom.document.getElementById("body").asInstanceOf[org.scalajs.dom.html.Body]
  private val championHeaderImg = dom.document.getElementById("championHeaderImg").asInstanceOf[Image]
  private val imgMap: Map[Int, String] =
    Map(1 -> "f", 2 -> "s", 3 -> "t", 4 -> "fo")
  private val colorMap: Map[Int, String] =
    Map(1 -> "grey", 2 -> "purple", 3 -> "red", 4 -> "blue",
      5 -> "green", 6 -> "yellow", 7 -> "red", 8 -> "blue")
  private val goldImg = dom.document.getElementById("goldImg").asInstanceOf[Image]
  private val silverImg = dom.document.getElementById("silverImg").asInstanceOf[Image]
  private val bronzeImg = dom.document.getElementById("bronzeImg").asInstanceOf[Image]
  private val crownImg = dom.document.getElementById("crownImg").asInstanceOf[Image]
  private val starImg = dom.document.getElementById("star").asInstanceOf[Image]
  private val bg1Img = dom.document.getElementById("bg1").asInstanceOf[Image]
  private val bg2Img = dom.document.getElementById("bg2").asInstanceOf[Image]
  private val bg3Img = dom.document.getElementById("bg3").asInstanceOf[Image]
  private val bg4Img = dom.document.getElementById("bg4").asInstanceOf[Image]
  private val bg5Img = dom.document.getElementById("bg5").asInstanceOf[Image]

  def resetScreen(): Unit = {
    windowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
    canvasUnit = (dom.window.innerWidth.toInt / window.x).toInt
    canvas.width = windowBoundary.x.toInt
    canvas.height = windowBoundary.y.toInt
    borderCanvas.width = canvasUnit * Boundary.w
    borderCanvas.height = canvasUnit * Boundary.h
    rankCanvas.width = dom.window.innerWidth.toInt
    rankCanvas.height = dom.window.innerHeight.toInt
    fieldScale = (-0.21)*canvasUnit + 7.6
    drawCache()
  }

  def drawGameOn(): Unit = {
    bodyAttribute.style_=("overflow:Scroll;overflow-y:hidden;overflow-x:hidden;")

    canvas.width = windowBoundary.x.toInt
    canvas.height = windowBoundary.y.toInt

    borderCanvas.width = canvasUnit * Boundary.w
    borderCanvas.height = canvasUnit * Boundary.h

    rankCanvas.width = dom.window.innerWidth.toInt
    rankCanvas.height = dom.window.innerHeight.toInt

    drawCache()

  }

  def drawCache(): Unit = {
    ctx.fillStyle = ColorsSetting.borderColor

    //画边界
    ctx.fillRect(0, 0, canvasUnit * BorderSize.w, canvasUnit)
    ctx.fillRect(0, 0, canvasUnit, canvasUnit * BorderSize.h)
    ctx.fillRect(0, BorderSize.h * canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
    ctx.fillRect(BorderSize.w * canvasUnit, 0, canvasUnit, canvasUnit * (BorderSize.h + 1))
  }

  def drawGameOff(firstCome: Boolean, replayFinish: Option[Boolean], loading: Boolean, readFileError: Boolean): Unit = {
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.drawImage(bg2Img,0,0,windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = "#000000"
    if (readFileError) {
      println("==============read file error")
      ctx.fillStyle = ColorsSetting.backgroundColor2
      canvas.width = 800
      canvas.height = 400
      ctx.fillRect(0, 0, 800.0, 400.0)
      ctx.fillStyle = "#000000"
      //      rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
      ctx.font = "36px Helvetica"
      ctx.fillText("文件不存在或文件已损坏...", 150, 180)
    } else if (replayFinish.nonEmpty && replayFinish.get) {
      rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
      ctx.font = "36px Helvetica"
      ctx.fillText("Replay ends.", 150, 180)
    } else if (loading) {
      rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
      ctx.font = "36px Helvetica"
      ctx.fillText("Loading......", 150, 180)
    } else {
      if (firstCome) {
        ctx.font = "36px Helvetica"
        ctx.fillText("Welcome.", 150, 180)
      } else {
        rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
        ctx.font = "36px Helvetica"
        ctx.fillText("Ops, connection lost.", 150, 180)
        ctx.fillText("Press Space Key To Restart!", 150, 250)
      }
    }
  }

  def drawServerShutDown(): Unit = {
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.drawImage(bg3Img,0,0,windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = "#000000"
    ctx.font = "36px Helvetica"
    ctx.fillText("Sorry, Some errors happened.", 150, 220)
  }

  def drawGameWait(): Unit = {
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.drawImage(bg5Img,0,0,windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = "#000000"
    ctx.font = "36px Helvetica"
    ctx.fillText("Please wait.", 150, 180)
  }


  def drawGameDie(killerOpt: Option[String], myScore: Score): Unit = {
    rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.drawImage(bg1Img,0,0,windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = "#000000"
    ctx.font = "200px Helvetica"
    ctx.scale(1, 1)
    val text = killerOpt match {
      case Some(killer) => s"Ops, You Are Killed By $killer! Press Space Key To Revenge!"
      case None => "Game Over"
    }
    val length = ctx.measureText(text).width
    val offx = length / 2
    val x = (dom.window.innerWidth / 2).toInt - 145
    val y = (dom.window.innerHeight / 2).toInt - 100
    val offy = (dom.window.innerHeight / 4).toInt
    //    val y = (dom.window.innerHeight / 2).toInt - 180

    ctx.fillText(text, dom.window.innerWidth / 2 - offx, offy) //(500,180)
    ctx.save()
    ctx.font = "bold 24px Helvetica"
    ctx.fillStyle = "#000000"
    ctx.fillText("YOUR SCORE:", x, y + 70)
    ctx.fillText(f"${myScore.score}" , x + 230, y + 70)
    ctx.restore()
  }



  def drawGameWin(myId: String, yourScore: Score, winnerScore: Score): Unit = {
    ctx.clearRect(0, 0, dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
    ctx.drawImage(bg5Img,0,0,windowBoundary.x, windowBoundary.y)
    rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
    ctx.save()
    ctx.scale(0.33, 0.33)
    ctx.restore()
    ctx.save()
    ctx.scale(1, 1)
    ctx.globalAlpha = 1
    ctx.font = "bold 30px Microsoft YaHei"
    ctx.fillStyle = "#000000"
    val txt2 = s"Press space to reStart"

    //    println(ctx.measureText(txt2).width.toString)
    ctx.font = "bold 24px Helvetica"
    ctx.fillStyle = "#000000"
    val txt4 = s"WINNER SCORE:" + f"${yourScore.score}"
    val length1 = ctx.measureText(txt4).width
    val txt3 = s"YOUR SCORE:" + f"${winnerScore.score}"
    ctx.fillText(txt3, (windowBoundary.x - length1) / 2, windowBoundary.y / 2)
    ctx.fillText(txt4, (windowBoundary.x - length1) / 2, windowBoundary.y / 2 + 40)
    ctx.font = "bold 20px Microsoft YaHei"
    ctx.fillText(txt2, dom.window.innerWidth.toFloat - 300, dom.window.innerHeight.toFloat - 100)
    ctx.font = "200px Helvetica"
    val txt5 = "Victory!"
    val offx = ctx.measureText(txt5).width / 2
    val offy = windowBoundary.y / 4
    ctx.fillText(txt5, dom.window.innerWidth / 2 - offx, offy )
    ctx.restore()
  }

  def drawBoundary(grid: Grid): Unit = {
    ctx.fillStyle = ColorsSetting.borderColor
    grid.boundaryMap.foreach{b =>
      ctx.fillRect(b.center.x * canvasUnit, b.center.y * canvasUnit, b.width * canvasUnit, b.height * canvasUnit)
    }
  }

  def drawBricks(bricks: Map[Point,Brick]):Unit = {
    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
//    ctx.drawImage(bg4Img,0,0,canvas.width,canvas.height)
    bricks.foreach{b =>
      val color = b._2.color
      val myColor = dom.document.getElementById(colorMap(color)).asInstanceOf[Image]
      if (b._2.hp > 0)
        ctx.drawImage(myColor,b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * brickWidth, canvasUnit * brickHeight)
      if (b._2.bonus == 1)
        ctx.drawImage(starImg,(b._1.x + brickWidth / 4) * canvasUnit, b._1.y * canvasUnit, canvasUnit * brickWidth / 2, canvasUnit * brickHeight)
      if ((color == 1 || color == 2) && b._2.hp == 1){
        ctx.fillStyle = "#000000"
        ctx.font = "bold 10px Helvetica"
        val txt = s"${b._2.hp}"
        ctx.fillText(txt, (b._1.x + brickWidth / 2.3 )  * canvasUnit, (b._1.y + brickHeight / 1.7) * canvasUnit)
      }

    }
  }
  def drawBoards(uid:String, offsetTime: Long, grid: Grid, frameRate: Int = 75):Unit = {
//    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
    grid.boardMap.foreach{ board =>
      val color = board._2.color
      val direction = board._2.direction * 2
      val off = direction * offsetTime.toFloat / frameRate
      ctx.fillStyle = color
      ctx.fillRect((board._2.center.x + off.x - board._2.length / 2) * canvasUnit ,board._2.center.y * canvasUnit, board._2.length * canvasUnit, boardHeight * canvasUnit)
      if (board._2.emotion != 0)
        {
          val myImg = dom.document.getElementById(imgMap(board._2.emotion)).asInstanceOf[Image]
          ctx.drawImage(myImg,(board._2.center.x + off.x - board._2.length / 2) * canvasUnit ,(board._2.center.y - 10 - ballRadius * 2) * canvasUnit , 10 * canvasUnit, 10 * canvasUnit)
        }
    }

  }


  def drawBalls(uid:String, offsetTime: Long, grid: Grid, frameRate: Int = 75):Unit = {
    val startTime = System.currentTimeMillis()
    grid.ballMap.foreach{ ball =>
      val color = ball._2.color
      val direction = if(ball._2.moveOrNot) ball._2.direction else ball._2.direction * 2
      val off = direction * offsetTime.toFloat / frameRate
      ctx.beginPath()
      ctx.fillStyle = color
      ctx.arc((ball._2.center.x + off.x) * canvasUnit , (ball._2.center.y + off.y) * canvasUnit, ballRadius * canvasUnit, 0, 2 * math.Pi)
      ctx.fill()
      ctx.closePath()
    }
    rankCtx.clearRect(20, textLineHeight * 5, rankCanvas.width / 4, textLineHeight * 2) //* 5, * 2
    PerformanceTool.renderFps(rankCtx, 20, textLineHeight, startTime)

  }




  def drawRank(uid: String, grid: Grid): Unit = {
    val currentRank = grid.scoreMap.values.toList.sortBy(_.score).reverse
    val currentNum = currentRank.length
    val personalScoreOp = grid.scoreMap.get(uid)
    if (personalScoreOp.isDefined) {
      val personalScore = personalScoreOp.get
      val personalRank =  currentRank.indexOf(personalScore) + 1
      val leftBegin = 20
      val rightBegin = windowBoundary.x - 230

      rankCtx.clearRect(0, 0, rankCanvas.width, rankCanvas.height) //绘制前清除canvas

      rankCtx.globalAlpha = 1
      rankCtx.textAlign = "left"
      rankCtx.textBaseline = "top"

      val myRankBaseLine = 4
      if (personalScore.id == uid) {
        val color = grid.boardMap.find(_._1 == uid).map(_._2.color).getOrElse(ColorsSetting.defaultColor)
        rankCtx.globalAlpha = 0.6
        rankCtx.fillStyle = color
        rankCtx.save()
        rankCtx.fillRect(leftBegin, (myRankBaseLine - 1) * textLineHeight, fillWidth , textLineHeight + 20)
        rankCtx.restore()

        rankCtx.globalAlpha = 1
        rankCtx.font = "22px Helvetica"
        rankCtx.fillStyle = ColorsSetting.fontColor2
        drawTextLine(f"${personalScore.score}", leftBegin, 0, myRankBaseLine)
      }

      val currentRankBaseLine = 2
      var index = 0
      rankCtx.font = "10px Helvetica"
      drawTextLine("Version:20190223", rightBegin.toInt+100, index, currentRankBaseLine-1)
      rankCtx.font = "14px Helvetica"
      drawTextLine(s" --- Current Rank ---   players:$currentNum", rightBegin.toInt, index, currentRankBaseLine)
      if (currentRank.lengthCompare(3) >= 0) {
        rankCtx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
        rankCtx.drawImage(silverImg, rightBegin - 5 - textLineHeight, textLineHeight * 3, textLineHeight, textLineHeight)
        rankCtx.drawImage(bronzeImg, rightBegin - 5 - textLineHeight, textLineHeight * 4, textLineHeight, textLineHeight)
      }
      else if (currentRank.lengthCompare(2) == 0) {
        rankCtx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
        rankCtx.drawImage(silverImg, rightBegin - 5 - textLineHeight, textLineHeight * 3, textLineHeight, textLineHeight)
      }
      else {
        rankCtx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
      }
      currentRank.foreach { score =>
        val color = grid.boardMap.find(_._1 == score.id).map(_._2.color).getOrElse(ColorsSetting.defaultColor)
        rankCtx.globalAlpha = 0.6
        rankCtx.fillStyle = color
        rankCtx.save()
        rankCtx.fillRect(windowBoundary.x - 20 - fillWidth , (index + currentRankBaseLine) * textLineHeight,
          fillWidth , textLineHeight)
        rankCtx.restore()

        rankCtx.globalAlpha = 1
        rankCtx.fillStyle = ColorsSetting.fontColor2
        index += 1
        drawTextLine(s"[$index]: ${score.name.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
        drawTextLine(s"area=" + f"${score.score}" + s"", rightBegin.toInt + 75, index, currentRankBaseLine)
      }

      index += 1
      val color = grid.boardMap.find(_._1 == personalScore.id).map(_._2.color).getOrElse(ColorsSetting.defaultColor)
      rankCtx.globalAlpha = 0.6
      rankCtx.fillStyle = color
      rankCtx.save()
      rankCtx.fillRect(windowBoundary.x - 20 - fillWidth, (index + currentRankBaseLine) * textLineHeight,
        fillWidth , textLineHeight)
      rankCtx.restore()

      rankCtx.globalAlpha = 1
      rankCtx.fillStyle = ColorsSetting.fontColor2
      index += 1
      drawTextLine(s"[$personalRank]: ${personalScore.name.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
      drawTextLine(s"area=" + f"${personalScore.score}" + s"", rightBegin.toInt + 75, index, currentRankBaseLine)

    }
  }


  def drawTextLine(str: String, x: Int, lineNum: Int, lineBegin: Int = 0): Unit = {
    rankCtx.fillText(str, x, (lineNum + lineBegin - 1) * textLineHeight)
  }

  def setScale(scale: Double, x: Double, y: Double): Unit = {
    ctx.translate(x, y)
    ctx.scale(scale, scale)
    ctx.translate(-x, -y)
  }

  def findLightColor(str: String): String = {
    val (r, g, b) = hex2Rgb(str)
    val newR = decToHex(r+20)
    val newG = decToHex(g+20)
    val newB = decToHex(b+20)
    s"#$newR$newG$newB"
  }

  def findDarkColor(str: String): String = {
    val (r, g, b) = hex2Rgb(str)
    val newR = decToHex(r-20)
    val newG = decToHex(g-20)
    val newB = decToHex(b-20)
    s"#$newR$newG$newB"
  }

  def hex2Rgb(hex: String):(Int, Int, Int) = {
    val red = hexToDec(hex.slice(1, 3))
    val green = hexToDec(hex.slice(3, 5))
    val blue = hexToDec(hex.takeRight(2))
    (red, green, blue)
  }

  def hexToDec(hex: String): Int = {
    val hexString: String = "0123456789ABCDEF"
    var target = 0
    var base = Math.pow(16, hex.length - 1).toInt
    for (i <- 0 until hex.length) {
      target = target + hexString.indexOf(hex(i)) * base
      base = base / 16
    }
    target
  }

  def decToHex(num: Int): String = {
    Integer.toHexString(num)
  }

  def isPointInWindow(p: Point4Trans, windowMax: Point, windowMin: Point): Boolean = {
    p.y < windowMax.y && p.y > windowMin.y && p.x > windowMin.x && p.x < windowMax.x
  }


}
