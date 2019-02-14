package org.seekloud.carnie.paperClient

import org.seekloud.carnie.common.Constant.ColorsSetting
import org.seekloud.carnie.paperClient.Protocol._
import org.seekloud.carnie.util.TimeTool
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
  private val smallMap = Point(littleMap.w, littleMap.h)
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
    Map(
      0 -> "luffyImg",
      1 -> "fatTigerImg",
      2 -> "BobImg",
      3 -> "yangImg",
      4 -> "smileImg",
      5 -> "pigImg"
    )
  //  private val myHeaderImg = dom.document.getElementById("myHeaderImg").asInstanceOf[Image]
  private val myHeaderImg = dom.document.getElementById(imgMap(img)).asInstanceOf[Image]
  //  private val otherHeaderImg = dom.document.getElementById("otherHeaderImg").asInstanceOf[Image]
  private val goldImg = dom.document.getElementById("goldImg").asInstanceOf[Image]
  private val silverImg = dom.document.getElementById("silverImg").asInstanceOf[Image]
  private val bronzeImg = dom.document.getElementById("bronzeImg").asInstanceOf[Image]
  private val killImg = dom.document.getElementById("killImg").asInstanceOf[Image]
  private val crownImg = dom.document.getElementById("crownImg").asInstanceOf[Image]
  //  private val backBtn = dom.document.getElementById("backBtn").asInstanceOf[Button]
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

  //  def drawRecordError(): Unit = {
  //    ctx.fillStyle = ColorsSetting.backgroundColor2
  //    ctx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
  //    ctx.fillStyle = ColorsSetting.fontColor
  //    ctx.font = "36px Helvetica"
  //    ctx.fillText("文件不存在或已损坏...", 150, 180)
  //  }

  def drawGameDie(killerOpt: Option[String], myScore: BaseScore, maxArea: Int, isReplay: Boolean = false): Unit = {
    //    backBtn.style.display="block"
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
    //绘制退出房间
    //    ctx.save()
    //    ctx.fillStyle = ColorsSetting.darkYellowColor
    //    ctx.fillRect(x, y + 250, 175, 60)
    //    ctx.font = "bold 24px Helvetica"
    //    ctx.fillStyle = ColorsSetting.fontColor2
    //    ctx.fillText("退出房间", x+40, y + 285)
    //    ctx.restore()
  }

  def drawBarrage(killedName: String, killerName: String) = {
    ctx.save()
    ctx.globalAlpha = 0.6
    ctx.restore()
    ctx.save()
    if (dom.window.innerWidth > 1200) ctx.font = "bold 20px Microsoft YaHei"
    else ctx.font = "bold 15px Microsoft YaHei"
    ctx.fillStyle = "#FF5809"
    val txt = s"$killedName is killed by $killerName"
    val length = ctx.measureText(txt).width
    val offx = length / 2
    ctx.fillText(s"$killedName is killed by $killerName", dom.window.innerWidth / 2 - offx, (dom.window.innerHeight / 9).toInt)
    ctx.restore()
  }

  def drawGameWin(myId: String, winner: String, data: FrontProtocol.WinData4Draw, winningData: WinData): Unit = {
    ctx.clearRect(0, 0, dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
    rankCtx.clearRect(0, 0, dom.window.innerWidth.toInt, dom.window.innerHeight.toInt)
    val winnerId = data.snakes.find(_.name == winner).map(_.id).get
    val snakes = data.snakes
    val snakesFields = data.fieldDetails
    val width = dom.window.innerWidth.toFloat - BorderSize.w * canvasUnit * 0.33
    val height = dom.window.innerHeight.toFloat - BorderSize.h * canvasUnit * 0.33
    ctx.save()
    ctx.scale(0.33, 0.33)
    ctx.fillStyle = ColorsSetting.borderColor
    ctx.fillRect(1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit * BorderSize.w, canvasUnit)
    ctx.fillRect(1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit, canvasUnit * BorderSize.h)
    ctx.fillRect(1.5 * width - canvasUnit, BorderSize.h * canvasUnit + 1.5 * height - canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
    ctx.fillRect(BorderSize.w * canvasUnit + 1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit, canvasUnit * (BorderSize.h + 1))
    snakesFields.foreach { field =>
      if (field.uid == myId || field.uid == winnerId) {
        val color = snakes.find(_.id == field.uid).map(_.color).get
        val darkC = findDarkColor(color)
        field.scanField.foreach { point =>
          point.y.foreach { y =>
            ctx.fillStyle = color
            ctx.fillRect(point.x * canvasUnit + 1.5 * width - canvasUnit, y._1 * canvasUnit + 1.5 * height - canvasUnit, canvasUnit * 1.1, canvasUnit * (y._2 - y._1 + 1))
            ctx.fillStyle = darkC
            ctx.fillRect(point.x * canvasUnit + 1.5 * width - canvasUnit, y._2 * canvasUnit + 1.5 * height - canvasUnit, canvasUnit * 1.05, canvasUnit * 0.3)
          }
        }
      }
    }
    ctx.restore()
    ctx.save()
    ctx.scale(1, 1)
    ctx.globalAlpha = 1
    ctx.font = "bold 30px Microsoft YaHei"
    ctx.fillStyle = "#000000"
    val txt1 = s"The Winner is $winner"
    val txt2 = s"Press space to reStart"

    //    println(ctx.measureText(txt2).width.toString)
    val length = ctx.measureText(txt1).width
    ctx.fillText(txt1, dom.window.innerWidth.toFloat / 2 - length / 2, 150)
    ctx.font = "bold 24px Helvetica"
    ctx.fillStyle = "#000000"
    val txt4 = s"WINNER SCORE:" + f"${winningData.winnerScore / canvasSize * 100}%.2f" + "%"
    val length1 = ctx.measureText(txt4).width
    if (winningData.yourScore.isDefined) {
      val txt3 = s"YOUR SCORE:" + f"${winningData.yourScore.get / canvasSize * 100}%.2f" + "%"
      ctx.fillText(txt3, (windowBoundary.x - length1) / 2, windowBoundary.y / 2)
    }
    ctx.fillText(txt4, (windowBoundary.x - length1) / 2, windowBoundary.y / 2 + 40)
    ctx.font = "bold 20px Microsoft YaHei"
    ctx.fillText(txt2, dom.window.innerWidth.toFloat - 300, dom.window.innerHeight.toFloat - 100)
    ctx.drawImage(crownImg, dom.window.innerWidth.toFloat / 2, 75, 50, 50)
    ctx.restore()
  }


  //  def drawField(fieldData: List[Protocol.FieldByColumn], snakes: List[SkDt]): Unit = {
  //    fieldCtx.clearRect(0, 0, fieldCanvas.width, fieldCanvas.height)
  //    fieldData.foreach { field => //按行渲染
  //      val color = snakes.find(_.id == field.uid).map(_.color).getOrElse(ColorsSetting.defaultColor)
  //      fieldCtx.fillStyle = color
  //      field.scanField.foreach { point =>
  //        point.x.foreach { x =>
  //          fieldCtx.fillRect(x._1 * canvasUnit, point.y * canvasUnit, canvasUnit * (x._2 - x._1 + 1), canvasUnit * 1.05)
  //        }
  //      }
  //    }
  //  }
//  var x = 0
//  def drawRect(): Unit = {
//    x += 0
//    val a = System.currentTimeMillis()
//    ctx.fillStyle = ColorsSetting.defaultColor
//    ctx.fillRect(0,0,100,100)
//    val b = System.currentTimeMillis()
//    if (x % 100 == 0)
//      println("a big rect" + (b - a))
//    val c = System.currentTimeMillis()
//    ctx.fillStyle = ColorsSetting.borderColor
//    for (i <- 0 to 99){
//      for (j <- 0 to 99){
//        ctx.fillRect(i + 200,j,1,1)
//      }
//    }
//    val d = System.currentTimeMillis()
//    if (x % 100 == 0)
//      println("many small rect" + (d - c))
//  }
  def drawBoundary(grid: Grid): Unit = {
    ctx.fillStyle = ColorsSetting.borderColor
    grid.boundaryMap.foreach{b =>
      ctx.fillRect(b.center.x, b.center.y, b.width * canvasUnit, b.height * canvasUnit)
    }
  }

  def drawBricks(bricks: Map[Point,Brick]):Unit = {
    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
    bricks.foreach{b =>
      val color = b._2.color
      ctx.fillStyle = color
      ctx.fillRect(b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * brickWidth, canvasUnit * brickHeight)
      ctx.fillStyle = findDarkColor(color)
      ctx.fillRect(b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * brickWidth, canvasUnit * 0.1)
      ctx.fillRect(b._1.x  * canvasUnit, b._1.y * canvasUnit, canvasUnit * 0.1, canvasUnit * brickHeight)
      ctx.fillRect((b._1.x + brickWidth - 0.1 )* canvasUnit, b._1.y * canvasUnit, canvasUnit * 0.1, canvasUnit * brickHeight)
      ctx.fillRect(b._1.x  * canvasUnit, (b._1.y + brickHeight - 0.1 ) * canvasUnit, canvasUnit * brickWidth, canvasUnit * 0.1)
    }
  }

  def drawBoards(uid:String, offsetTime: Long, grid: Grid, frameRate: Int = 75):Unit = {
//    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
    grid.boardMap.foreach{ board =>
      val color = board._2.color
      val direction = board._2.direction
      val off = direction * offsetTime.toFloat / frameRate * 2
      ctx.fillStyle = color
//      println(board._2.id,board._2.center)
//      if(if (board._2.direction.x >= board._2.center.x) board._2.center.x + off.x <= board._2.direction.x
//      else board._2.center.x + off.x >= board._2.direction.x)
        ctx.fillRect((board._2.center.x + off.x - boardWidth / 2) * canvasUnit ,board._2.center.y * canvasUnit, boardWidth * canvasUnit, boardHeight * canvasUnit)
    }

  }

  def drawBalls(uid:String, offsetTime: Long, grid: Grid, frameRate: Int = 75):Unit = {
    //    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
    grid.ballMap.foreach{ ball =>
      val color = ball._2.color
      val direction = ball._2.direction
      val off = direction * offsetTime.toFloat / frameRate * 2
      ctx.beginPath()
      ctx.fillStyle = color
//      println(ball._2.id,ball._2.center , direction)
      //      if(if (board._2.direction.x >= board._2.center.x) board._2.center.x + off.x <= board._2.direction.x
      //      else board._2.center.x + off.x >= board._2.direction.x)
      ctx.arc((ball._2.center.x + off.x) * canvasUnit , (ball._2.center.y + off.y) * canvasUnit, ballRadius * canvasUnit, 0, 2 * math.Pi)
      ctx.fill()
      ctx.closePath()
    }

  }


  def drawGrid(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid,
               championId: String, isReplay: Boolean = false, frameRate: Int = 150,
               newFieldInfo: Option[List[FieldByColumn]] = None,fieldByX:List[FrontProtocol.Field4Draw] = List.empty): Unit = { //头所在的点是屏幕的正中心
    //    println(s"drawGrid-frameRate: $frameRate")
    val startTime = System.currentTimeMillis()
    val snakes = data.snakes
    //    val trueFrame = if(mode ==1) Protocol.frameRate2 else Protocol.frameRate1

    val lastHeader = snakes.find(_.id == uid) match {
      case Some(s) =>
        val nextDirection = grid.nextDirection(s.id).getOrElse(s.direction)
        val direction = if (s.direction + nextDirection != Point(0, 0)) nextDirection else s.direction
        s.header + direction * offsetTime.toFloat / frameRate

      case None =>
        Point(border.x / 2, border.y / 2)
    }

    val offx = window.x / 2 - lastHeader.x //新的框的x偏移量
    val offy = window.y / 2 - lastHeader.y //新的框的y偏移量

    val newWindowBorder = Point(window.x / scale.toFloat, window.y / scale.toFloat)
    val (minPoint, maxPoint) = (lastHeader - newWindowBorder, lastHeader + newWindowBorder)

    ctx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)

    val snakeWithOff = data.snakes.map(i => i.copy(header = Point(i.header.x + offx, y = i.header.y + offy)))

    var fieldByXInWindow: List[FrontProtocol.Field4Draw] = Nil

    data.fieldDetails.foreach { user =>
      //      if (snakes.exists(_.id == user.uid)) {
      if (!snakes.exists(_.id == user.uid)) {
        println(s"snakes don't exist fieldId-${user.uid}")
      }
      var userScanField: List[FrontProtocol.Scan4Draw] = Nil
      user.scanField.foreach { field =>
        if (field.x < maxPoint.x + 10 && field.x > minPoint.x - 5) {
          userScanField = FrontProtocol.Scan4Draw(field.x, field.y.filter(y => y._1 < maxPoint.y || y._2 > minPoint.y)) :: userScanField
        }
      }
      fieldByXInWindow = FrontProtocol.Field4Draw(user.uid, userScanField) :: fieldByXInWindow
      //      }
    }

    val bodyInWindow = data.bodyDetails.filter{b =>
      if(!snakes.exists(_.id == b.uid)) {
        println(s"snakes don't exist bodyId-${b.uid}")
      }
      b.turn.exists(p => isPointInWindow(p, maxPoint, minPoint))
    }

    scale = Math.max(1 - grid.getMyFieldCount(uid, fieldByXInWindow.filter(_.uid==uid).flatMap(_.scanField)) * 0.00002, 0.94)
    ctx.save()
    setScale(scale, windowBoundary.x / 2, windowBoundary.y / 2)
    ctx.globalAlpha = 1.0
    val w = windowBoundary.x / 2
    val h = windowBoundary.y / 2
    val radialGradient1 = ctx.createRadialGradient(w, h, 100, w, h,1000)

    radialGradient1.addColorStop(0, "#FFFFFF")

    radialGradient1.addColorStop(1, "#87CEFA")

    ctx.fillStyle = radialGradient1

    ctx.fillRect(0, 0, w * 2 + 50, h * + 50)
//    ctx.fillStyle = "#B4DCFF"
//    ctx.fillRect(0,0,windowBoundary.x,windowBoundary.y)
    //边界
    ctx.drawImage(borderCanvas, offx * canvasUnit, offy * canvasUnit)
    ctx.restore()

    ctx.globalAlpha = 0.6
    bodyInWindow.foreach { bds =>
      val color = snakes.find(_.id == bds.uid).map(_.color).getOrElse(ColorsSetting.defaultColor)
      ctx.fillStyle = color
      val turnPoints = bds.turn
      (0 until turnPoints.length - 1).foreach { i => //拐点渲染
        val start = turnPoints(i)
        val end = turnPoints(i + 1)
        if (start.x == end.x) { //同x
          if (start.y > end.y) {
            ctx.fillRect((start.x + offx) * canvasUnit, (end.y + 1 + offy) * canvasUnit, canvasUnit, (start.y - end.y) * canvasUnit)
          } else {
            ctx.fillRect((start.x + offx) * canvasUnit, (start.y + offy) * canvasUnit, canvasUnit, (end.y - start.y) * canvasUnit)
          }
        } else { // 同y
          if (start.x > end.x) {
            ctx.fillRect((end.x + 1 + offx) * canvasUnit, (end.y + offy) * canvasUnit, (start.x - end.x) * canvasUnit, canvasUnit)
          } else {
            ctx.fillRect((start.x + offx) * canvasUnit, (start.y + offy) * canvasUnit, (end.x - start.x) * canvasUnit, canvasUnit)
          }
        }
      }
      if (turnPoints.nonEmpty) ctx.fillRect((turnPoints.last.x + offx) * canvasUnit, (turnPoints.last.y + offy) * canvasUnit, canvasUnit, canvasUnit)
    }

    ctx.globalAlpha = offsetTime.toFloat / frameRate * 1
    if(newFieldInfo.nonEmpty){
      var newFieldInWindow: List[FieldByColumn] = Nil
      newFieldInfo.get.foreach { user =>
        if (snakes.exists(_.id == user.uid)) {
          newFieldInWindow = FieldByColumn(user.uid, user.scanField.map { field =>
            ScanByColumn(field.y.filter(y => y._1 < maxPoint.y || y._2 > minPoint.y),
              field.x.filter(x => x._1 < maxPoint.x || x._2 > minPoint.x))
          }) :: newFieldInWindow
        }
      }

      newFieldInWindow.foreach { field =>
        val color = snakes.find(_.id == field.uid).map(_.color).getOrElse(ColorsSetting.defaultColor)
        ctx.fillStyle = color
        field.scanField.foreach { fids =>
          fids.y.foreach { y =>
            fids.x.foreach { x =>
              ctx.fillRect((x._1 + offx) * canvasUnit, (y._1 + offy) * canvasUnit, canvasUnit * (x._2 - x._1 + 1), canvasUnit * (y._2 - y._1 + 1.04))
            }
          }
        }
      }
    }


    ctx.globalAlpha = 1.0

    fieldByXInWindow.foreach { field => //按行渲染
      val color = snakes.find(_.id == field.uid).map(_.color).getOrElse(ColorsSetting.defaultColor)
      val darkC = findDarkColor(color)

      field.scanField.foreach { point =>
        point.y.foreach { y =>
          ctx.fillStyle = color
          ctx.fillRect((point.x + offx) * canvasUnit, (y._1 + offy) * canvasUnit, canvasUnit * 1.05 , canvasUnit * (y._2 - y._1 + 1))
          ctx.fillStyle = darkC
          ctx.fillRect((point.x + offx) * canvasUnit, (y._2 + offy + 1 - 0.3) * canvasUnit, canvasUnit * 1.02 , canvasUnit * 0.3)
        }
      }
    }

//    fieldInWindow.foreach { field => //按行渲染
//      val color = snakes.find(_.id == field.uid).map(_.color).getOrElse(ColorsSetting.defaultColor)
//      ctx.fillStyle = color
//
//      field.scanField.foreach { fids =>
//        fids.y.foreach { y =>
//          fids.x.foreach { x =>
//            ctx.fillRect((x._1 + offx) * canvasUnit, (y._1 + offy) * canvasUnit, canvasUnit * (x._2 - x._1 + 1), canvasUnit * (y._2 - y._1 + 1.05))
//          }
//        }
//      }
//    }

    snakeWithOff.foreach { s => //draw headers
      ctx.fillStyle = s.color

      val nextDirection = grid.nextDirection(s.id).getOrElse(s.direction)
      val direction = if (s.direction + nextDirection != Point(0, 0)) nextDirection else s.direction
      val off = direction * offsetTime.toFloat / frameRate
      ctx.fillRect((s.header.x + off.x) * canvasUnit, (s.header.y + off.y - 0.3) * canvasUnit, canvasUnit, canvasUnit)

//      val otherHeaderImg = dom.document.getElementById(imgMap(s.img)).asInstanceOf[Image]
//      val img = if (s.id == uid) myHeaderImg else otherHeaderImg

      if (s.id == championId)
        ctx.drawImage(championHeaderImg, (s.header.x + off.x) * canvasUnit, (s.header.y + off.y - 1 - 0.3) * canvasUnit, canvasUnit, canvasUnit) //头部图片绘制在名字上方
//      ctx.drawImage(img, (s.header.x + off.x) * canvasUnit, (s.header.y + off.y) * canvasUnit, canvasUnit, canvasUnit) //头部图片绘制在名字上方

      val lightC = findLightColor(s.color)
      val darkC = findDarkColor(s.color)
//      println(s"color-${s.color},lightC-$lightC,darkC-$darkC")
      ctx.fillStyle = lightC
      ctx.fillRect((s.header.x + off.x) * canvasUnit, (s.header.y + off.y - 0.3) * canvasUnit, canvasUnit, canvasUnit)
      ctx.fillStyle = darkC
      ctx.fillRect((s.header.x + off.x) * canvasUnit, (s.header.y + off.y + 1 - 0.3) * canvasUnit, canvasUnit, 0.3 * canvasUnit)

      ctx.font = "16px Helvetica"
      ctx.fillStyle = "#000000"
      ctx.fillText(s.name, (s.header.x + off.x) * canvasUnit + canvasUnit / 2 - ctx.measureText(s.name).width / 2, (s.header.y + off.y - 1 - 0.3) * canvasUnit - 3) //-10
    }



    //    //排行榜
    if (!isReplay) {
      rankCtx.clearRect(20, textLineHeight * 5, rankCanvas.width / 4, textLineHeight * 2) //* 5, * 2
      PerformanceTool.renderFps(rankCtx, 20, textLineHeight, startTime)
    }
  }

  def drawSmallMap(myHeader: Point, otherSnakes: List[SkDt]): Unit = {
    val offx = myHeader.x.toDouble / border.x * smallMap.x
    val offy = myHeader.y.toDouble / border.y * smallMap.y
    ctx.fillStyle = ColorsSetting.mapColor
    val w = canvas.width * 0.99 - littleMap.w * canvasUnit //* 1.042
    val h = canvas.height * 0.985 - littleMap.h * canvasUnit //* 1.030
    ctx.save()
    ctx.globalAlpha = 0.5
    ctx.fillRect(w.toInt, h.toInt, littleMap.w * canvasUnit + 5, littleMap.h * canvasUnit + 5)
    ctx.restore()
    ctx.drawImage(myHeaderImg, (w + offx * canvasUnit).toInt, (h + offy * canvasUnit).toInt, 10, 10)
    otherSnakes.foreach { i =>
      val x = i.header.x.toDouble / border.x * smallMap.x
      val y = i.header.y.toDouble / border.y * smallMap.y
      ctx.fillStyle = i.color
      ctx.fillRect(w + x * canvasUnit, h + y * canvasUnit, 10, 10)
    }
    ctx.font = "12px Helvetica"
    ctx.fillStyle = ColorsSetting.fontColor2
    ctx.fillText(s"${TimeTool.dateFormatDefault(System.currentTimeMillis())}", w.toInt, h.toInt)
  }

  def drawRank(uid: String, snakes: List[SkDt], currentRank: List[Score], personalScoreOp: Option[Score], personalRankOp: Option[Byte], currentNum: Byte): Unit = {
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

  def drawRank4Replay(uid: String, snakes: List[SkDt], currentRank: List[Score]): Unit = {

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
