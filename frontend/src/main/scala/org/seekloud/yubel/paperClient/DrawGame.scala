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
    Map(1 -> "grey", 2 -> "purple", 3 -> "red", 4 -> "blue", 5 -> "green", 6 -> "yellow")
  private val goldImg = dom.document.getElementById("goldImg").asInstanceOf[Image]
  private val silverImg = dom.document.getElementById("silverImg").asInstanceOf[Image]
  private val bronzeImg = dom.document.getElementById("bronzeImg").asInstanceOf[Image]
  private val killImg = dom.document.getElementById("killImg").asInstanceOf[Image]
  private val crownImg = dom.document.getElementById("crownImg").asInstanceOf[Image]
  private var scale = 1.0

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

  def drawVerifyErr(): Unit = {
    canvas.width = windowBoundary.x.toInt
    canvas.height = windowBoundary.y.toInt
    ctx.fillStyle = ColorsSetting.backgroundColor2
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = ColorsSetting.fontColor
    ctx.font = "36px Helvetica"
    ctx.fillText(s"It failed to verify the player's info!", 150, 180)
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
    ctx.fillStyle = ColorsSetting.backgroundColor2
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = ColorsSetting.fontColor
    if (readFileError) {
      println("==============read file error")
      ctx.fillStyle = ColorsSetting.backgroundColor2
      canvas.width = 800
      canvas.height = 400
      ctx.fillRect(0, 0, 800.0, 400.0)
      ctx.fillStyle = ColorsSetting.fontColor
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
    ctx.fillStyle = ColorsSetting.backgroundColor2
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = ColorsSetting.fontColor
    ctx.font = "36px Helvetica"
    ctx.fillText("Sorry, Some errors happened.", 150, 180)
  }

  def drawGameDie4Replay(): Unit = {
    ctx.fillStyle = ColorsSetting.backgroundColor2
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = ColorsSetting.fontColor
    ctx.fillStyle = ColorsSetting.backgroundColor2
    canvas.width = 800
    canvas.height = 400
    ctx.fillRect(0, 0, 800.0, 400.0)
    ctx.fillStyle = ColorsSetting.fontColor
    //      rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
    ctx.font = "36px Helvetica"
    ctx.fillText("您观看的玩家已死亡...", 150, 180)
  }

  def drawGameWait(): Unit = {
    ctx.fillStyle = ColorsSetting.backgroundColor2
    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
    ctx.fillStyle = ColorsSetting.fontColor
    ctx.font = "36px Helvetica"
    ctx.fillText("Please wait.", 150, 180)
  }


  def drawGameDie(killerOpt: Option[String], myScore: BaseScore, maxArea: Int, isReplay: Boolean = false): Unit = {
    rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
//    ctx.fillStyle = ColorsSetting.backgroundColor2
    //    ctx.fillStyle = ColorsSetting.backgroundColor
//    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
//    ctx.fillStyle = ColorsSetting.gameNameColor
    ctx.fillStyle = ColorsSetting.fontColor3

    ctx.font = "24px Helvetica"
    ctx.scale(1, 1)

    val text = killerOpt match {
      case Some(killer) => s"Ops, You Are Killed By $killer! Press Space Key To Revenge!"
      case None => "Ops, Press Space Key To Restart!"
    }

    val length = ctx.measureText(text).width
    val offx = length / 2
    val x = (dom.window.innerWidth / 2).toInt - 145
    val y = if (isReplay) (dom.window.innerHeight / 2).toInt - 80 else (dom.window.innerHeight / 2).toInt - 100
    //    val y = (dom.window.innerHeight / 2).toInt - 180

    val gameTime = myScore.playTime
    val bestScore = maxArea / canvasSize * 100
    val time = {
      val tempM = gameTime / 60
      val s1 = gameTime % 60
      val s = if (s1 < 0) "00" else if (s1 < 10) "0" + s1 else s1.toString
      val m = if (tempM < 0) "00" else if (tempM < 10) "0" + tempM else tempM.toString
      m + ":" + s
    }
    ctx.fillText(text, dom.window.innerWidth / 2 - offx, y) //(500,180)
    ctx.save()
    ctx.font = "bold 24px Helvetica"
//    ctx.fillStyle = ColorsSetting.fontColor
    ctx.fillStyle = ColorsSetting.fontColor3
    ctx.fillText("YOUR SCORE:", x, y + 70)
    ctx.fillText(f"${myScore.area / canvasSize * 100}%.2f" + "%", x + 230, y + 70)
    ctx.fillText("BEST SCORE:", x, y + 110)
    ctx.fillText(f"$bestScore%.2f" + "%", x + 230, y + 110)
    ctx.fillText(s"PLAYERS KILLED:", x, y + 150)
    ctx.fillText(s"${myScore.kill}", x + 230, y + 150)
    if (!isReplay) {
      ctx.fillText(s"TIME PLAYED:", x, y + 190)
      ctx.fillText(s"$time", x + 230, y + 190)
    }
    ctx.restore()
  }



  def drawGameWin(myId: String, score: Score): Unit = {
    ctx.clearRect(0, 0, dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
    rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
    val width = dom.window.innerWidth.toFloat - BorderSize.w * canvasUnit * 0.33
    val height = dom.window.innerHeight.toFloat - BorderSize.h * canvasUnit * 0.33
    ctx.save()
    ctx.scale(0.33, 0.33)
    ctx.fillStyle = ColorsSetting.borderColor
    ctx.fillRect(1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit * BorderSize.w, canvasUnit)
    ctx.fillRect(1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit, canvasUnit * BorderSize.h)
    ctx.fillRect(1.5 * width - canvasUnit, BorderSize.h * canvasUnit + 1.5 * height - canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
    ctx.fillRect(BorderSize.w * canvasUnit + 1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit, canvasUnit * (BorderSize.h + 1))
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
    val txt4 = s"WINNER SCORE:" + f"${score.score}" + "%"
    val length1 = ctx.measureText(txt4).width
//    if (winningData.yourScore.isDefined) {
//      val txt3 = s"YOUR SCORE:" + f"${winningData.yourScore.get / canvasSize * 100}%.2f" + "%"
//      ctx.fillText(txt3, (windowBoundary.x - length1) / 2, windowBoundary.y / 2)
//    }
    ctx.fillText(txt4, (windowBoundary.x - length1) / 2, windowBoundary.y / 2 + 40)
    ctx.font = "bold 20px Microsoft YaHei"
    ctx.fillText(txt2, dom.window.innerWidth.toFloat - 300, dom.window.innerHeight.toFloat - 100)
    ctx.drawImage(crownImg, dom.window.innerWidth.toFloat / 2, 75, 50, 50)
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
    bricks.foreach{b =>
      val color = b._2.color
      val myColor = dom.document.getElementById(colorMap(color)).asInstanceOf[Image]
      if (b._2.hp > 0)
      ctx.drawImage(myColor,b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * brickWidth, canvasUnit * brickHeight)
//      ctx.fillStyle = color
//      ctx.fillRect(b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * brickWidth, canvasUnit * brickHeight)
//      ctx.fillStyle = findDarkColor(color)
//      ctx.fillRect(b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * brickWidth, canvasUnit * 0.1)
//      ctx.fillRect(b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * 0.1, canvasUnit * brickHeight)
//      ctx.fillRect((b._1.x + brickWidth - 0.1 )* canvasUnit, b._1.y * canvasUnit, canvasUnit * 0.1, canvasUnit * brickHeight)
//      ctx.fillRect(b._1.x  * canvasUnit, (b._1.y + brickHeight - 0.1 ) * canvasUnit, canvasUnit * brickWidth, canvasUnit * 0.1)
    }
  }

  def drawBoards(uid:String, offsetTime: Long, grid: Grid, frameRate: Int = 75):Unit = {
//    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
    grid.boardMap.foreach{ board =>
      val color = board._2.color
      val direction = board._2.direction * 2
      val off = direction * offsetTime.toFloat / frameRate
      ctx.fillStyle = color
//      println(board._2.id,board._2.center)
//      if(if (board._2.direction.x >= board._2.center.x) board._2.center.x + off.x <= board._2.direction.x
//      else board._2.center.x + off.x >= board._2.direction.x)
      ctx.fillRect((board._2.center.x + off.x - getBoardWidth / 2) * canvasUnit ,board._2.center.y * canvasUnit, getBoardWidth * canvasUnit, boardHeight * canvasUnit)
      if (board._2.emotion != 0)
        {
          val myImg = dom.document.getElementById(imgMap(board._2.emotion)).asInstanceOf[Image]
          ctx.drawImage(myImg,(board._2.center.x + off.x - getBoardWidth / 2) * canvasUnit ,(board._2.center.y - 10 - ballRadius * 2) * canvasUnit , 10 * canvasUnit, 10 * canvasUnit)
        }
    }

  }


  def drawBalls(uid:String, offsetTime: Long, grid: Grid, frameRate: Int = 75):Unit = {
    //    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
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

      //      val baseLine = 2
      //      rankCtx.font = "22px Helvetica"
      //      rankCtx.fillStyle = ColorsSetting.fontColor2
      //      drawTextLine(s"KILL: ", leftBegin, 0, baseLine)
      //      rankCtx.drawImage(killImg, leftBegin + 55, textLineHeight, textLineHeight * 1.4, textLineHeight * 1.4)
      //      drawTextLine(s" x ${personalScore.k}", leftBegin + 55 + (textLineHeight * 1.4).toInt, 0, baseLine)


      val myRankBaseLine = 4
      if (personalScore.id == uid) {
        val color = grid.boardMap.find(_._1 == uid).map(_._2.color).getOrElse(ColorsSetting.defaultColor)
        rankCtx.globalAlpha = 0.6
        rankCtx.fillStyle = color
        rankCtx.save()
        rankCtx.fillRect(leftBegin, (myRankBaseLine - 1) * textLineHeight, fillWidth , textLineHeight + 10)
        rankCtx.restore()

        rankCtx.globalAlpha = 1
        rankCtx.font = "22px Helvetica"
        rankCtx.fillStyle = ColorsSetting.fontColor2
        drawTextLine(f"${personalScore.score}" + s"%", leftBegin, 0, myRankBaseLine)
      }

      val currentRankBaseLine = 2
      var index = 0
      rankCtx.font = "10px Helvetica"
      drawTextLine("Version:20190219", rightBegin.toInt+100, index, currentRankBaseLine-1)
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

  def drawRankBefore(uid: String, snakes: List[SkDt], currentRank: List[Sc], personalScoreOp: Option[Sc], personalRankOp: Option[Byte], currentNum: Byte): Unit = {
    val personalScore = if (personalScoreOp.isDefined) personalScoreOp.get else currentRank.filter(_.id == uid).head
    val personalRank = if (personalRankOp.isDefined) personalRankOp.get else currentRank.indexOf(personalScore) + 1
    val leftBegin = 20
    val rightBegin = windowBoundary.x - 230

    rankCtx.clearRect(0, 0, rankCanvas.width, rankCanvas.height) //绘制前清除canvas

    rankCtx.globalAlpha = 1
    rankCtx.textAlign = "left"
    rankCtx.textBaseline = "top"

    val baseLine = 2
    rankCtx.font = "22px Helvetica"
    rankCtx.fillStyle = ColorsSetting.fontColor2
    drawTextLine(s"KILL: ", leftBegin, 0, baseLine)
    rankCtx.drawImage(killImg, leftBegin + 55, textLineHeight, textLineHeight * 1.4, textLineHeight * 1.4)
    drawTextLine(s" x ${personalScore.k}", leftBegin + 55 + (textLineHeight * 1.4).toInt, 0, baseLine)


    val myRankBaseLine = 4
    if (personalScore.id == uid) {
      val color = snakes.find(_.id == uid).map(_.color).getOrElse(ColorsSetting.defaultColor)
      rankCtx.globalAlpha = 0.6
      rankCtx.fillStyle = color
      rankCtx.save()
      rankCtx.fillRect(leftBegin, (myRankBaseLine - 1) * textLineHeight, fillWidth + windowBoundary.x / 8 * (personalScore.area.toDouble / canvasSize), textLineHeight + 10)
      rankCtx.restore()

      rankCtx.globalAlpha = 1
      rankCtx.font = "22px Helvetica"
      rankCtx.fillStyle = ColorsSetting.fontColor2
      drawTextLine(f"${personalScore.area.toDouble / canvasSize * 100}%.2f" + s"%", leftBegin, 0, myRankBaseLine)
    }

    val currentRankBaseLine = 2
    var index = 0
    rankCtx.font = "10px Helvetica"
    drawTextLine("Version:20190121", rightBegin.toInt+100, index, currentRankBaseLine-1)
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
      val color = snakes.find(_.id == score.id).map(_.color).getOrElse(ColorsSetting.defaultColor)
      rankCtx.globalAlpha = 0.6
      rankCtx.fillStyle = color
      rankCtx.save()
      rankCtx.fillRect(windowBoundary.x - 20 - fillWidth - windowBoundary.x / 8 * (score.area.toDouble / canvasSize), (index + currentRankBaseLine) * textLineHeight,
        fillWidth + windowBoundary.x / 8 * (score.area.toDouble / canvasSize), textLineHeight)
      rankCtx.restore()

      rankCtx.globalAlpha = 1
      rankCtx.fillStyle = ColorsSetting.fontColor2
      index += 1
      drawTextLine(s"[$index]: ${score.n.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
      drawTextLine(s"area=" + f"${score.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 75, index, currentRankBaseLine)
      drawTextLine(s"kill=${score.k}", rightBegin.toInt + 160, index, currentRankBaseLine)
    }

    index += 1
    val color = snakes.find(_.id == personalScore.id).map(_.color).getOrElse(ColorsSetting.defaultColor)
    rankCtx.globalAlpha = 0.6
    rankCtx.fillStyle = color
    rankCtx.save()
    rankCtx.fillRect(windowBoundary.x - 20 - fillWidth - windowBoundary.x / 8 * (personalScore.area.toDouble / canvasSize), (index + currentRankBaseLine) * textLineHeight,
      fillWidth + windowBoundary.x / 8 * (personalScore.area.toDouble / canvasSize), textLineHeight)
    rankCtx.restore()

    rankCtx.globalAlpha = 1
    rankCtx.fillStyle = ColorsSetting.fontColor2
    index += 1
    drawTextLine(s"[$personalRank]: ${personalScore.n.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
    drawTextLine(s"area=" + f"${personalScore.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 75, index, currentRankBaseLine)
    drawTextLine(s"kill=${personalScore.k}", rightBegin.toInt + 160, index, currentRankBaseLine)
  }

  def drawRank4Replay(uid: String, snakes: List[SkDt], currentRank: List[Sc]): Unit = {

    val leftBegin = 20
    val rightBegin = windowBoundary.x - 230

    rankCtx.clearRect(0, 0, rankCanvas.width, rankCanvas.height) //绘制前清除canvas

    rankCtx.globalAlpha = 1
    rankCtx.textAlign = "left"
    rankCtx.textBaseline = "top"

    val mySnake = snakes.filter(_.id == uid).head
    val baseLine = 2
    rankCtx.font = "22px Helvetica"
    rankCtx.fillStyle = ColorsSetting.fontColor2
    drawTextLine(s"KILL: ", leftBegin, 0, baseLine)
    rankCtx.drawImage(killImg, leftBegin + 55, textLineHeight, textLineHeight * 1.4, textLineHeight * 1.4)
    drawTextLine(s" x ${mySnake.kill}", leftBegin + 55 + (textLineHeight * 1.4).toInt, 0, baseLine)


    val myRankBaseLine = 4
    currentRank.filter(_.id == uid).foreach { score =>
      val color = snakes.find(_.id == uid).map(_.color).getOrElse(ColorsSetting.defaultColor)
      rankCtx.globalAlpha = 0.6
      rankCtx.fillStyle = color
      rankCtx.save()
      rankCtx.fillRect(leftBegin, (myRankBaseLine - 1) * textLineHeight, fillWidth + windowBoundary.x / 8 * (score.area.toDouble / canvasSize), textLineHeight + 10)
      rankCtx.restore()

      rankCtx.globalAlpha = 1
      rankCtx.font = "22px Helvetica"
      rankCtx.fillStyle = ColorsSetting.fontColor2
      drawTextLine(f"${score.area.toDouble / canvasSize * 100}%.2f" + s"%", leftBegin, 0, myRankBaseLine)
    }
    val currentRankBaseLine = 2
    var index = 0
    rankCtx.font = "14px Helvetica"
    drawTextLine(s" --- Current Rank --- ", rightBegin.toInt, index, currentRankBaseLine)
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
      val color = snakes.find(_.id == score.id).map(_.color).getOrElse(ColorsSetting.defaultColor)
      rankCtx.globalAlpha = 0.6
      rankCtx.fillStyle = color
      rankCtx.save()
      rankCtx.fillRect(windowBoundary.x - 20 - fillWidth - windowBoundary.x / 8 * (score.area.toDouble / canvasSize), (index + currentRankBaseLine) * textLineHeight,
        fillWidth + windowBoundary.x / 8 * (score.area.toDouble / canvasSize), textLineHeight)
      rankCtx.restore()

      rankCtx.globalAlpha = 1
      rankCtx.fillStyle = ColorsSetting.fontColor2
      index += 1
      drawTextLine(s"[$index]: ${score.n.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
      drawTextLine(s"area=" + f"${score.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 70, index, currentRankBaseLine)
      drawTextLine(s"kill=${score.k}", rightBegin.toInt + 160, index, currentRankBaseLine)
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

  def findLightColor(str: String) = {
    val (r, g, b) = hex2Rgb(str)
    val newR = decToHex(r+20)
    val newG = decToHex(g+20)
    val newB = decToHex(b+20)
    s"#$newR$newG$newB"
  }

  def findDarkColor(str: String) = {
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

  def decToHex(num: Int) = {
    Integer.toHexString(num)
  }

  def isPointInWindow(p: Point4Trans, windowMax: Point, windowMin: Point): Boolean = {
    p.y < windowMax.y && p.y > windowMin.y && p.x > windowMin.x && p.x < windowMax.x
  }


}
