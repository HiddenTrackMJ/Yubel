package org.seekloud.yubel.scene

//import java.awt.Graphics
//import java.io.File

import java.awt.image.BufferedImage
import java.nio.ByteBuffer

import org.seekloud.yubel.paperClient._
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.yubel.common.BotAppSetting.isGray
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.{Image, WritableImage}
import javafx.scene.paint.Color
import javafx.scene.text.{Font, FontPosture, FontWeight, Text}
import org.seekloud.yubel.common.Constant
import org.seekloud.yubel.common.Constant.ColorsSetting
import javafx.embed.swing.SwingFXUtils
import javafx.geometry.VPos
import javafx.scene.SnapshotParameters
//import javafx.scene.media.{AudioClip, AudioEqualizer, Media, MediaPlayer}
import org.slf4j.LoggerFactory
import com.google.protobuf.ByteString
import org.seekloud.esheepapi.pb.observations.ImgData

/**
  * Created by dry on 2018/10/29.
  **/
class LayeredCanvas(viewCanvas: Canvas,rankCanvas: Canvas,positionCanvas: Canvas,BorderCanvas: Canvas,
  selfViewCanvas: Canvas,selfCanvas: Canvas,humanViewCanvas:Canvas,headerCanvas:Canvas, img: Int) {//,background: BackgroundCanvas
  private val realWindow = Point(selfViewCanvas.getWidth.toFloat, selfViewCanvas.getHeight.toFloat)
  private val window = Point(Window.w, Window.h)
  private val border = Point(BorderSize.w, BorderSize.h)
  private val windowBoundary = Point(viewCanvas.getWidth.toFloat, viewCanvas.getHeight.toFloat)
  private val humanWindowBoundary = Point(humanViewCanvas.getWidth.toFloat, humanViewCanvas.getHeight.toFloat - 210 )
  private val positionWindowBoundary = Point(positionCanvas.getWidth.toFloat, positionCanvas.getHeight.toFloat)
  
  private val positionCanvasUnit = positionWindowBoundary.x / border.x
  private var positionCanvasUnitY = positionWindowBoundary.y / border.y

  private val viewCtx = viewCanvas.getGraphicsContext2D
  private val rankCtx = rankCanvas.getGraphicsContext2D
  private val positionCtx = positionCanvas.getGraphicsContext2D
  private val BorderCtx = BorderCanvas.getGraphicsContext2D
  private val selfViewCtx = selfViewCanvas.getGraphicsContext2D
  private val selfCtx = selfCanvas.getGraphicsContext2D
  private val humanViewCtx = humanViewCanvas.getGraphicsContext2D
  private val headerCtx = headerCanvas.getGraphicsContext2D
  
  private val canvasSize = (border.x - 1) * (border.y - 1)
  private val imgMap: Map[Int, Image] =
    Map(
      0 -> new Image("img/luffy.png"),
      1 -> new Image("img/fatTiger.png"),
      2 -> new Image("img/Bob.png"),
      3 -> new Image("img/yang.png"),
      4 -> new Image("img/smile.png"),
      5 -> new Image("img/pig.png")
    )
  private val championHeaderImg = new Image("champion.png")
  private val myHeaderImg = imgMap(img)
  private val crownImg = new Image("crown.png")
  private val canvasUnit = (positionWindowBoundary.x / window.x).toDouble
  private val humanCanvasUnit = (humanWindowBoundary.x / window.x).toDouble
  private var humanCanvasUnitY = (humanWindowBoundary.y / window.y).toDouble
  private var canvasUnitY =   (positionWindowBoundary.y / window.y).toDouble
  private var scale = 1.0
  private val smallMap = Point(littleMap.w, littleMap.h)
  private val textLineHeight = 15
  private val textLineHeightH = 10
  private[this] val log = LoggerFactory.getLogger(this.getClass)

  def debug(str: String):Unit = log.debug(s"$str")

  var a=0

  def getAllImageData ={
    val canvasList = List(humanViewCanvas,positionCanvas,BorderCanvas,headerCanvas,viewCanvas,selfViewCanvas,selfCanvas,rankCanvas)
    canvasList.map(c => getImageData(c))
  }

  def getImageData(canvas: Canvas) = {
//    a += 1
    var height = canvas.getHeight.toInt
    if (canvas.getId == "7") height -= 210
    val width = canvas.getWidth.toInt
    val params = new SnapshotParameters
    val id = canvas.getId
    params.setFill(Color.TRANSPARENT)

    val writableImage = canvas.snapshot(params,null)
//    val bufferedImage = new BufferedImage(width,height,2)
//    SwingFXUtils.fromFXImage(writableImage,bufferedImage)
//    val argb = bufferedImage.getRGB(0,0,width,height,null,0,width)
//    val byteBuffer = ByteBuffer.allocate(4*width*height)
//    argb.foreach{i =>
//      byteBuffer.putInt(i)
//    }
//    byteBuffer.flip()
//    val byteArray = byteBuffer.array().take(byteBuffer.limit())
    val reader = writableImage.getPixelReader
    val wIm = new WritableImage(width,height)
    val writer = wIm.getPixelWriter
    val data =
      if(!isGray) {
        //获取彩图，每个像素点4Byte
        val byteBuffer = ByteBuffer.allocate(4 * width * height)
        byteBuffer.clear()
        for (y <- 0 until height; x <- 0 until width) {
          val color = reader.getArgb(x, y)
          writer.setArgb(x,y,color)
          byteBuffer.putInt(color)
        }
        byteBuffer.flip() //翻转，修改lim为当前pos，pos置0
        val arrayOfByte = byteBuffer.array().take(byteBuffer.limit) //take前limit个Byte，去除Buffer内余下的多余数据
        ByteString.copyFrom(arrayOfByte)
      } else {
        //获取灰度图，每个像素点1Byte
        val byteArray = new Array[Byte](1 * width * height)
        for (y <- 0 until height; x <- 0 until width) {
          val color = reader.getColor(x, y).grayscale()
          val gray = color.getRed * color.getOpacity
          writer.setColor(x,y,new Color(gray,gray,gray,color.getOpacity))
          byteArray(y * height + x) = (gray * 255).toByte
        }
        ByteString.copyFrom(byteArray)
      }
//    if (canvas.getId == "2"){
//      humanViewCtx.drawImage(wIm, 10, 10)
//        println(s"${data.toByteArray.toList.distinct}")
//    }
    val pixelLength = if(isGray) 1 else 4
    (id, ImgData(width ,height, pixelLength, data))
  }

  def drawPosition(myHeader: Point,championHeader: Option[Point],isMe: Boolean):Unit = {
//    val offx = myHeader.x.toDouble / border.x * window.x
//    val offy = myHeader.y.toDouble / border.y * window.y
    val offx = myHeader.x.toDouble * positionCanvasUnit
    val offy = myHeader.y.toDouble * positionCanvasUnit
    positionCtx.setFill(Color.BLACK)
    val w = positionWindowBoundary.x //400
    val h = positionWindowBoundary.y //300
    positionCtx.clearRect(0,0,w,h)
    a += 1
//    if(a % 200 ==0) println(w,h,positionCanvasUnit)
    positionCtx.save()
//    positionCtx.setGlobalAlpha(0.5)
    positionCtx.fillRect(0, 0, w , h )
    positionCtx.restore()

//    positionCtx.setFill(Color.rgb(105,105,105))
//    positionCtx.fillRect(positionCanvasUnit, positionCanvasUnit, positionCanvasUnit * border.x, positionCanvasUnit)
//    positionCtx.fillRect(positionCanvasUnit, positionCanvasUnit, positionCanvasUnit, positionCanvasUnit * border.y)
//    positionCtx.fillRect(positionCanvasUnit, border.y * positionCanvasUnit, positionCanvasUnit * border.x , positionCanvasUnit)
//    positionCtx.fillRect(border.x  * positionCanvasUnit, positionCanvasUnit, positionCanvasUnit, positionCanvasUnit * border.y )
//    positionCtx.restore()

    positionCtx.setFill(Color.WHITE)
    positionCtx.fillRect( offx - window.x / 2 * positionCanvasUnit , offy  - window.y / 2 * positionCanvasUnit, window.x * positionCanvasUnit, window.y * positionCanvasUnit)
//    if(isMe){
//      positionCtx.drawImage(championHeaderImg, offx - 5, offy - 5, 10, 10)
//    }
//    else{
//      positionCtx.drawImage(myHeaderImg, offx - 5, offy - 5, 10, 10)
//      if(championHeader.isDefined){
//        positionCtx.drawImage(championHeaderImg, championHeader.get.x*positionCanvasUnit - 5, championHeader.get.y*positionCanvasUnit - 5, 10, 10)
//      }
//    }
//    val params = new SnapshotParameters
//    params.setFill(Color.TRANSPARENT)
//    viewCtx.drawImage(positionCanvas.snapshot(params,null), 10, 10)
//    if(a % 20 ==0)getImageData(humanViewCanvas)

  }

  def drawBorder(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int): Unit = {

    val (snakeWithOff, fieldInWindow, bodyInWindow, offx, offy) =
      findOffset(BorderCanvas, uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int)

    BorderCtx.setFill(Color.BLACK)
    val w = positionWindowBoundary.x //400
    val h = positionWindowBoundary.y //300
    BorderCtx.fillRect(0, 0, w , h )
    BorderCtx.save()

    BorderCtx.setFill(Color.rgb(105,105,105))
    //  //画边界
    BorderCtx.fillRect(canvasUnit * offx, canvasUnit * offy, canvasUnit * BorderSize.w, canvasUnit)
    BorderCtx.fillRect(canvasUnit * offx, canvasUnit * offy, canvasUnit, canvasUnit * BorderSize.h)
    BorderCtx.fillRect(canvasUnit * offx, (BorderSize.h + offy) * canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
    BorderCtx.fillRect((BorderSize.w + offx) * canvasUnit, canvasUnit * offy, canvasUnit, canvasUnit * (BorderSize.h + 1))
//    BorderCtx.save()
  }

//  def drawGameWin(myId: String, winner: String, data: Data4TotalSync,winningData:WinData): Unit = {
//    val winnerId = data.snakes.find(_.name == winner).map(_.id).get
//    val snakes = data.snakes
//    val snakesFields = data.fieldDetails
//    scale = 0.33
//    val width = windowBoundary.x - BorderSize.w * canvasUnit * scale
//    val height = windowBoundary.y - BorderSize.h * canvasUnit * scale
//    selfViewCtx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
//    selfViewCtx.save()
//    selfViewCtx.scale(scale, scale)
//    selfViewCtx.setFill(ColorsSetting.borderColor)
//    selfViewCtx.fillRect(1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit * BorderSize.w, canvasUnit)
//    selfViewCtx.fillRect(1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit, canvasUnit * BorderSize.h)
//    selfViewCtx.fillRect(1.5 * width - canvasUnit, BorderSize.h * canvasUnit + 1.5 * height - canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
//    selfViewCtx.fillRect(BorderSize.w * canvasUnit + 1.5 * width - canvasUnit, 1.5 * height - canvasUnit, canvasUnit, canvasUnit * (BorderSize.h + 1))
//    snakesFields.foreach { field =>
//      if (field.uid == myId || field.uid == winnerId) {
//        val color = snakes.find(_.id == field.uid).map(s => Constant.hex2Rgb(s.color)).get
//        selfViewCtx.setFill(color)
//        field.scanField.foreach { point =>
//          point.x.foreach { x =>
//            selfViewCtx.fillRect(x._1 * canvasUnit + 1.5 * width - canvasUnit, point.y * canvasUnit + 1.5 * height - canvasUnit, canvasUnit * (x._2 - x._1 + 1), canvasUnit * 1.05)
//          }
//        }
//      }
//    }
//    selfViewCtx.restore()
//    selfViewCtx.save()
//    selfViewCtx.scale(1, 1)
//    selfViewCtx.setGlobalAlpha(1)
//    selfViewCtx.setFont(Font.font("Microsoft YaHei", FontPosture.findByName("bold"), 25))
//    selfViewCtx.setFill(ColorsSetting.defaultColor)
//
//    val txt1 = s"The Winner is $winner"
//    val txt2 = s"Press space to reStart"
//    val length = new Text(txt1).getLayoutBounds.getWidth
//    selfViewCtx.fillText(txt1, (windowBoundary.x - length) / 2 , windowBoundary.y / 5)
//    selfViewCtx.setFont(Font.font("Microsoft YaHei", FontWeight.BOLD, 20))
//    //    ctx.setfont = "bold 24px Helvetica"
//    selfViewCtx.setFill(ColorsSetting.fontColor2)
//    val txt4 = s"WINNER SCORE:" + f"${winningData.winnerScore / canvasSize * 100}%.2f" + "%"
//    val length1 = new Text(txt4).getLayoutBounds.getWidth
//    if(winningData.yourScore.isDefined) {
//      val txt3 = s"YOUR SCORE:" + f"${winningData.yourScore.get / canvasSize * 100}%.2f" + "%"
//      selfViewCtx.fillText(txt3, (windowBoundary.x - length1) / 2 , windowBoundary.y / 4)
//    }
//    selfViewCtx.fillText(txt4, (windowBoundary.x - length1) / 2 , windowBoundary.y / 4 + 40)
//    selfViewCtx.setFont(Font.font("Microsoft YaHei", FontWeight.BOLD, 20)) //FontPosture.findByName("bold")
//    selfViewCtx.fillText(txt2, windowBoundary.x - 300, windowBoundary.y - 100)
//    selfViewCtx.drawImage(crownImg, (windowBoundary.x - length) / 2 + length - 50, windowBoundary.y / 5 - 75, 50, 50)
//    selfViewCtx.restore()
//  }

  import javafx.scene.text.Text
//  def drawUserDieInfo(killedName: String, killerName: String): Unit = {
//    selfViewCtx.save()
//    //    ctx.globalAlpha = 0.6
//
//    selfViewCtx.restore()
//    selfViewCtx.save()
//    selfViewCtx.setFont(Font.font(30))
//    selfViewCtx.setFill(ColorsSetting.gameNameColor)
//    val txt = s"$killedName is killed by $killerName"
//    val text = new Text(txt)
//    text.setFont(Font.font(30))
//    text.setFill(ColorsSetting.gameNameColor)
//    val length = text.getLayoutBounds.getWidth
//    val offx = length / 2
//    //    ctx.drawImage(bloodImg, windowBoundary.x / 2 - offx, 115, 300, 50)
//    selfViewCtx.fillText(s"$killedName is killed by $killerName", windowBoundary.x / 2 - offx, 150)
//    selfViewCtx.restore()
//  }

//  def drawGameDie(killerOpt: Option[String],  myScore :BaseScore, maxArea: Int): Unit = {
//    //    rankCtx.clearRect(0, 0, windowBoundary.x, windowBoundary.y)
//    //    val endTime = System.currentTimeMillis()
//    //    if (myScore.area > maxArea) maxArea = myScore.area
//    selfViewCtx.setFill(ColorsSetting.dieInfoBackgroundColor)
//    selfViewCtx.fillRect(0, 0, windowBoundary.x, windowBoundary.y)
//    selfViewCtx.setFill(ColorsSetting.gameNameColor)
//    selfViewCtx.setFont(Font.font(20))
//    scale = 1
//    selfViewCtx.scale(1, 1)
//
//    val text = killerOpt match {
//      case Some(killer) => s"Ops, You Are Killed By $killer! Press Space Key To Revenge!"
//      case None => "Ops, Press Space Key To Restart!"
//    }
//    val txt =new Text(text)
//    val length = txt.getLayoutBounds.getWidth
//    val offx = length / 2
//    val x = (windowBoundary.x / 2).toInt - 145
//    val y = (windowBoundary.y / 2).toInt - 180
//
//    val gameTime = myScore.playTime
//    val bestScore = maxArea / canvasSize * 100
//    val time = {
//      val tempM = gameTime / 60
//      val s1 = gameTime % 60
//      val s = if (s1 < 0) "00" else if (s1 < 10) "0" + s1 else s1.toString
//      val m = if (tempM < 0) "00" else if (tempM < 10) "0" + tempM else tempM.toString
//      m + ":" + s
//    }
//    selfViewCtx.fillText(text, windowBoundary.x / 2 - offx - 50 , y) //(500,180)
//    selfViewCtx.save()
//    selfViewCtx.setFill(ColorsSetting.dieInfoFontColor)
//    selfViewCtx.setFont(Font.font(20))
//    selfViewCtx.fillText("YOUR SCORE:", x, y + 70)
//    selfViewCtx.fillText(f"${myScore.area / canvasSize * 100}%.2f" + "%", x + 230, y + 70)
//    selfViewCtx.fillText("BEST SCORE:", x, y + 110)
//    selfViewCtx.fillText(f"$bestScore%.2f" + "%", x + 230, y + 110)
//    selfViewCtx.fillText(s"PLAYERS KILLED:", x, y + 150)
//    selfViewCtx.fillText(s"${myScore.kill}", x + 230, y + 150)
//    selfViewCtx.fillText(s"TIME PLAYED:", x, y + 190)
//    selfViewCtx.fillText(s"$time", x + 230, y + 190)
//    selfViewCtx.restore()
//  }

//  def drawDieBarrage(killedName: String, killerName: String): Unit = {
//    selfViewCtx.save()
//    selfViewCtx.setGlobalAlpha(0.6)
//    //    ctx.drawImage(bloodImg, 670, 115, 300, 50)
//    selfViewCtx.restore()
//    selfViewCtx.save()
//    selfViewCtx.setFill(Color.rgb(255, 88, 9))
//    selfViewCtx.setFont(Font.font(25))
//    val length = 30
//    val offx = (270 - length) / 2
//    selfViewCtx.fillText(s"$killedName is killed by $killerName", 670 + offx, 150)
//    selfViewCtx.restore()
//  }
  
  def drawCache(offx: Float, offy: Float): Unit = { //离屏缓存的更新--缓存边界
    //    ctx.clearRect(0,0,canvas.getWidth,canvas.getHeight)
    selfViewCtx.setFill(Color.rgb(105,105,105))

    //画边界
    selfViewCtx.fillRect(canvasUnit * offx, canvasUnit * offy, canvasUnit * BorderSize.w, canvasUnit)
    selfViewCtx.fillRect(canvasUnit * offx, canvasUnit * offy, canvasUnit, canvasUnit * BorderSize.h)
    selfViewCtx.fillRect(canvasUnit * offx, (BorderSize.h + offy) * canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
    selfViewCtx.fillRect((BorderSize.w + offx) * canvasUnit, canvasUnit * offy, canvasUnit, canvasUnit * (BorderSize.h + 1))
  }

  def drawHumanMap(myHeader: Point, Snakes: List[SkDt], championId: String):Unit={
    val offx = myHeader.x.toDouble / border.x * smallMap.x
    val offy = myHeader.y.toDouble / border.y * smallMap.y
    humanViewCtx.setFill(ColorsSetting.mapColor)
    val w = humanWindowBoundary.x * 0.99 - littleMap.w * humanCanvasUnit //* 1.100
    val h = humanWindowBoundary.y - littleMap.h * humanCanvasUnit - 10//* 1.170
//    a = a + 1
//    if(a % 10 == 0) println("w:" +w +"h:"+h)
    humanViewCtx.save()
    humanViewCtx.setGlobalAlpha(0.5)
    humanViewCtx.fillRect(w, h, littleMap.w * humanCanvasUnit + 10, littleMap.h * humanCanvasUnit + 10)
    humanViewCtx.restore()
//    humanViewCtx.drawImage(myHeaderImg, w + offx * humanCanvasUnit, h + offy * humanCanvasUnit, 10, 10)
    Snakes.foreach { s =>
      val x = s.header.x.toDouble / border.x * smallMap.x
      val y = s.header.y.toDouble / border.y * smallMap.y
      humanViewCtx.setFill(Constant.hex2Rgb(s.color))
//      humanViewCtx.fillRect(w + x * humanCanvasUnit, h + y * humanCanvasUnit, 10, 10)


      val color = Constant.hex2Rgb(s.color)
      humanViewCtx.setFill(color)
      humanViewCtx.fillRect(w + x * humanCanvasUnit, h + y * humanCanvasUnit, 10, 10)
      val lightC = findLightColor(s.color)
      val darkC = findDarkColor(s.color)
      humanViewCtx.setFill(Constant.hex2Rgb(lightC))
      humanViewCtx.fillRect(w + x * humanCanvasUnit, h + y * humanCanvasUnit, 10, 10)
      humanViewCtx.setFill(Constant.hex2Rgb(darkC))
      humanViewCtx.fillRect(w + x * humanCanvasUnit, h + 10 + y * humanCanvasUnit, 10, 0.1 * 10)
      if (s.id == championId)
        humanViewCtx.drawImage(championHeaderImg, w + x * humanCanvasUnit, h + y * humanCanvasUnit, 10, 10) //头部图片绘制在名字上方
    }
  }
  
  private val goldImg = new Image("gold.png")
  private val silverImg = new Image("silver.png")
  private val bronzeImg = new Image("bronze.png")
  private val killImg = new Image("kill.png")

  private val fillWidth = 33
  private var lastRankNumH = 0 //清屏用
  private val myRankBaseLineH = 4



  def drawHumanView(currentRank: List[Score],uid: String, data: FrontProtocol.Data4Draw,
                    offsetTime: Long, grid: Grid, frameRate: Int,myActions: Map[Int,Int],
                    championId: String,personalScoreOp: Option[Score],
                    personalRankOp: Option[Byte], currentNum: Byte): Unit = { //头所在的点是屏幕的正中心

    val personalScore = if (personalScoreOp.isDefined) personalScoreOp.get else currentRank.filter(_.id == uid).head
    val personalRank = if (personalRankOp.isDefined) personalRankOp.get else currentRank.indexOf(personalScore) + 1
    val snakes = data.snakes

    humanViewCtx.clearRect(0, 0, humanWindowBoundary.x, humanWindowBoundary.y)

    val (snakeWithOff, fieldInWindow, bodyInWindow, offX, offY) =
      findOffset(humanViewCanvas, uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int,isHuman = true)

//    scale = 1 - grid.getMyFieldCount(uid, fieldInWindow.filter(_.uid==uid).flatMap(_.scanField)) * 0.00008
    humanViewCtx.save()

    humanViewCtx.setFill(Color.rgb(105,105,105))
    humanViewCtx.fillRect(humanCanvasUnit * offX, humanCanvasUnit * offY, humanCanvasUnit * BorderSize.w, humanCanvasUnit)
    humanViewCtx.fillRect(humanCanvasUnit * offX, humanCanvasUnit * offY, humanCanvasUnit, humanCanvasUnit * BorderSize.h)
    humanViewCtx.fillRect(humanCanvasUnit * offX, (BorderSize.h + offY) * humanCanvasUnit, humanCanvasUnit * (BorderSize.w + 1), humanCanvasUnit)
    humanViewCtx.fillRect((BorderSize.w + offX) * humanCanvasUnit, humanCanvasUnit * offY, humanCanvasUnit, humanCanvasUnit * (BorderSize.h + 1))

    drawForBody(bodyInWindow, humanViewCanvas, snakes, offX, offY, canvasUnit = humanCanvasUnit)

    drawForField(fieldInWindow, humanViewCanvas, snakes, offX, offY, canvasUnit = humanCanvasUnit)

//    drawForHeader(snakeWithOff.filter(_.id == uid), humanViewCanvas, uid, offsetTime, grid, frameRate, canvasUnit = humanCanvasUnit)

    snakeWithOff.foreach { s =>
      humanViewCtx.setFill(Constant.hex2Rgb(s.color))

      val nextDirection = grid.nextDirection(s.id).getOrElse(s.direction)
      val direction = if (s.direction + nextDirection != Point(0, 0)) nextDirection else s.direction
      val off = direction * offsetTime.toFloat / frameRate
      humanViewCtx.fillRect((s.header.x + off.x) * humanCanvasUnit, (s.header.y + off.y) * humanCanvasUnit, humanCanvasUnit, humanCanvasUnit)
      if (s.id == championId)
        humanViewCtx.drawImage(championHeaderImg, (s.header.x + off.x) * humanCanvasUnit, (s.header.y + off.y - 1) * humanCanvasUnit, humanCanvasUnit, humanCanvasUnit)
//      val otherHeaderImg = imgMap(s.img)
//      val img = if (s.id == uid) myHeaderImg else otherHeaderImg
//      humanViewCtx.drawImage(img, (s.header.x + off.x) * humanCanvasUnit, (s.header.y + off.y) * humanCanvasUnit, humanCanvasUnit, humanCanvasUnit)

      val color = Constant.hex2Rgb(s.color)
      humanViewCtx.setFill(color)
      humanViewCtx.fillRect((s.header.x + off.x) * humanCanvasUnit, (s.header.y + off.y - 0.3) * humanCanvasUnit, humanCanvasUnit, humanCanvasUnit)
      val lightC = findLightColor(s.color)
      val darkC = findDarkColor(s.color)
      humanViewCtx.setFill(Constant.hex2Rgb(lightC))
      humanViewCtx.fillRect((s.header.x + off.x) * humanCanvasUnit, (s.header.y + off.y - 0.3) * humanCanvasUnit, humanCanvasUnit, humanCanvasUnit)
      humanViewCtx.setFill(Constant.hex2Rgb(darkC))
      humanViewCtx.fillRect((s.header.x + off.x) * humanCanvasUnit, (s.header.y + off.y + 1 - 0.3) * humanCanvasUnit, humanCanvasUnit, 0.3 * humanCanvasUnit)

      humanViewCtx.setFont(Font.font(16))
      humanViewCtx.setFill(Color.rgb(0, 0, 0))
      val t = new Text(s"${s.name}")
      humanViewCtx.fillText(s.name, (s.header.x + off.x) * humanCanvasUnit + humanCanvasUnit / 2 - t.getLayoutBounds.getWidth / 2, (s.header.y + off.y - 1) * humanCanvasUnit - 3)
    }

    val leftBegin = 20
    val rightBegin = humanWindowBoundary.x - 230

    lastRankNumH = currentRank.length

        humanViewCtx.setGlobalAlpha(1.0)
        humanViewCtx.setTextBaseline(VPos.TOP)

    val mySnake = snakes.filter(_.id == uid).head
    val baseLine = 2
    humanViewCtx.setFont(Font.font(20))
    humanViewCtx.setFill(Color.rgb(0,0,0))
    drawTextLine(s"KILL: ", leftBegin, 0, baseLine)
    humanViewCtx.drawImage(killImg, leftBegin + 55, textLineHeight, textLineHeight * 1.4, textLineHeight * 1.4)
    drawTextLine(s" x ${mySnake.kill}", leftBegin + 55 + (textLineHeight * 1.4).toInt, 0, baseLine)

    if (personalScore.id == uid) {
      val color = snakes.find(_.id == uid).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
      humanViewCtx.setGlobalAlpha(0.6)
      humanViewCtx.setFill(color)
      humanViewCtx.save()
      humanViewCtx.fillRect(leftBegin, (myRankBaseLineH - 1) * textLineHeight, fillWidth + humanWindowBoundary.x / 8 * (personalScore.area.toDouble / canvasSize), textLineHeight + 10)
      humanViewCtx.restore()

      humanViewCtx.setGlobalAlpha(1)
      humanViewCtx.setFont(Font.font(20))
      humanViewCtx.setFill(Color.rgb(0,0,0))
      drawTextLine(f"${personalScore.area.toDouble / canvasSize * 100}%.2f" + s"%", leftBegin, 0, myRankBaseLineH)
    }

    val currentRankBaseLine = 2
    var index = 0
    humanViewCtx.setFont(Font.font(14))

    drawTextLine(s" --- Current Rank ---   players:$currentNum", rightBegin.toInt, index, currentRankBaseLine)
    if (currentRank.lengthCompare(3) >= 0) {
      humanViewCtx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
      humanViewCtx.drawImage(silverImg, rightBegin - 5 - textLineHeight, textLineHeight * 3, textLineHeight, textLineHeight)
      humanViewCtx.drawImage(bronzeImg, rightBegin - 5 - textLineHeight, textLineHeight * 4, textLineHeight, textLineHeight)
    }
    else if (currentRank.lengthCompare(2) == 0) {
      humanViewCtx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
      humanViewCtx.drawImage(silverImg, rightBegin - 5 - textLineHeight, textLineHeight * 3, textLineHeight, textLineHeight)
    }
    else {
      humanViewCtx.drawImage(goldImg, rightBegin - 5 - textLineHeight, textLineHeight * 2, textLineHeight, textLineHeight)
    }
    currentRank.foreach { score =>
      val color = snakes.find(_.id == score.id).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
      humanViewCtx.setGlobalAlpha(0.6)
      humanViewCtx.setFill(color)
      humanViewCtx.save()
      humanViewCtx.fillRect(humanWindowBoundary.x - 20 - fillWidth - humanWindowBoundary.x / 8 * (score.area.toDouble / canvasSize), (index + currentRankBaseLine) * textLineHeight,
        fillWidth + humanWindowBoundary.x / 8 * (score.area.toDouble / canvasSize), textLineHeight)
      humanViewCtx.restore()

      humanViewCtx.setGlobalAlpha(1)
      humanViewCtx.setFill(Color.rgb(0,0,0))
      index += 1
      drawTextLine(s"[$index]: ${score.n.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
      drawTextLine(s"area=" + f"${score.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 70, index, currentRankBaseLine)
      drawTextLine(s"kill=${score.k}", rightBegin.toInt + 160, index, currentRankBaseLine)
    }

    drawTextLine(s"[$personalRank]: ${personalScore.n.+("   ").take(3)}", rightBegin.toInt, index + 2, currentRankBaseLine)
    drawTextLine(s"area=" + f"${personalScore.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 70, index + 2, currentRankBaseLine)
    drawTextLine(s"kill=${personalScore.k}", rightBegin.toInt + 160, index + 2 , currentRankBaseLine)

    humanViewCtx.restore()

    humanViewCtx.clearRect(0, 400, 800 , 210)
    humanViewCtx.setFill(Color.BLACK)
    humanViewCtx.save()
    humanViewCtx.fillRect(0, 405, 395 , 210 )
    humanViewCtx.restore()
//    humanViewCtx.setFill(Color.WHITE)
//    humanViewCtx.save()
//    humanViewCtx.fillRect(0, 405, 395 , 210 )
//    humanViewCtx.restore()

    val baseLine4A = 2
    var index4A = 0
    if(myActions.size >= 12){
      myActions.toList.sortBy(_._1).takeRight(12).foreach{ a =>
        humanViewCtx.setFill(Color.WHITE)
        humanViewCtx.save()
        humanViewCtx.fillText(s"frame:${a._1},action:${a._2}",20,400 + (index4A + baseLine4A) * textLineHeight)
        index4A += 1
      }
    }
    else {
      myActions.toList.sortBy(_._1).foreach{ a =>
        humanViewCtx.setFill(Color.WHITE)
        humanViewCtx.save()
        humanViewCtx.fillText(s"frame:${a._1},action:${a._2}",20,400 + (index4A + baseLine4A) * textLineHeight)
        index4A += 1
      }
    }

  }
  
  def drawSelfView(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int, championId: String): Unit = { //头所在的点是屏幕的正中心
    
    val snakes = data.snakes

    val (snakeWithOff, fieldInWindow, bodyInWindow, offx, offy) =
      findOffset(selfViewCanvas, uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int)

    selfViewCtx.save()

    drawForBody(bodyInWindow, selfViewCanvas, snakes, offx, offy)

    drawForField(fieldInWindow, selfViewCanvas, snakes, offx, offy)

    drawForHeader(snakeWithOff, selfViewCanvas, uid, offsetTime, grid, frameRate,championId)

//    drawCache(offx , offy)

  }

  def drawSelf(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int, championId: String): Unit = { //头所在的点是屏幕的正中心
    val snakes = data.snakes

    val (snakeWithOff, fieldInWindow, bodyInWindow, offx, offy) =
      findOffset(selfCanvas, uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int)

    //    scale = Math.max(1 - grid.getMyFieldCount(uid, fieldInWindow.filter(_.uid==uid).flatMap(_.scanField)) * 0.00002, 0.94)
    selfCtx.save()

    //    setScale(scale, windowBoundary.x / 2, windowBoundary.y / 2)
    drawForBody(bodyInWindow.filter(_.uid == uid), selfCanvas, snakes, offx, offy)

    drawForField(fieldInWindow.filter(_.uid == uid), selfCanvas, snakes, offx, offy)

    drawForHeader(snakeWithOff.filter(_.id == uid), selfCanvas, uid, offsetTime, grid, frameRate,championId)

//    setScale(scale, windowBoundary.x / 2, windowBoundary.y / 2)
//    selfCtx.setFill(Color.rgb(105,105,105))
//
//  //画边界
//    selfCtx.fillRect(canvasUnit * offx, canvasUnit * offy, canvasUnit * BorderSize.w, canvasUnit)
//    selfCtx.fillRect(canvasUnit * offx, canvasUnit * offy, canvasUnit, canvasUnit * BorderSize.h)
//    selfCtx.fillRect(canvasUnit * offx, (BorderSize.h + offy) * canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
//    selfCtx.fillRect((BorderSize.w + offx) * canvasUnit, canvasUnit * offy, canvasUnit, canvasUnit * (BorderSize.h + 1))

  }
  
  def drawHeader(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int, championId: String): Unit = {
    val (snakeWithOff, fieldInWindow, bodyInWindow, offx, offy) =
      findOffset(headerCanvas, uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int)

    //    scale = Math.max(1 - grid.getMyFieldCount(uid, fieldInWindow.filter(_.uid==uid).flatMap(_.scanField)) * 0.00002, 0.94)
    headerCtx.save()

    //    setScale(scale, windowBoundary.x / 2, windowBoundary.y / 2)

    drawForHeader(snakeWithOff, headerCanvas, uid, offsetTime, grid, frameRate, championId)

  }

  def drawBody(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int, championId: String):Unit ={
    val snakes = data.snakes
    val (snakeWithOff, fieldInWindow, bodyInWindow, offx, offy) =
      findOffset(viewCanvas, uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, grid: Grid, frameRate: Int)

//    scale = Math.max(1 - grid.getMyFieldCount(uid, fieldInWindow.filter(_.uid==uid).flatMap(_.scanField)) * 0.00002, 0.94)
    viewCtx.save()

    //    setScale(scale, windowBoundary.x / 2, windowBoundary.y / 2)
    drawForBody(bodyInWindow, viewCanvas, snakes, offx, offy)

    drawForField(fieldInWindow, viewCanvas, snakes, offx, offy)

    drawForHeader(snakeWithOff, viewCanvas, uid, offsetTime, grid, frameRate, championId)

  }

  private var lastRankNum = 0 //清屏用
  def drawRank(uid: String, snakes: List[SkDt], currentRank: List[Score]): Unit = {
//    val leftBegin = 20
    val rightBegin = 20
//    val maxArea = if(0.4 - currentRank.length * 0.5 > 0.15) 0.4 - currentRank.length * 0.5 else 0.15
    val maxArea = 0.10
    val ratioLen = 170
    val areaUnit = ratioLen / maxArea
    val killUnit = if(currentRank.nonEmpty){if(currentRank.map(_.k).max != 0)ratioLen / currentRank.map(_.k).max else ratioLen}
                       else 0

//    drawClearRank()//绘制前清除canvas

    lastRankNum = currentRank.length

    rankCtx.setGlobalAlpha(1.0)
    rankCtx.setTextBaseline(VPos.TOP)

    val w = positionWindowBoundary.x //400
    val h = positionWindowBoundary.y //300
    rankCtx.clearRect(0, 0, w, h)
    rankCtx.setFill(Color.BLACK)
    rankCtx.fillRect(0, 0, w , h )
    rankCtx.save()

    val currentRankBaseLine = 1
    var index = 0
    currentRank.foreach { score =>
      val color = snakes.find(_.id == score.id).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
      rankCtx.setGlobalAlpha(0.6)
      rankCtx.setFill(color)
      rankCtx.save()
//      rankCtx.fillRect(rightBegin.toInt, (2 * index + currentRankBaseLine) * textLineHeightH,
//        0 + areaUnit * (score.area.toDouble / canvasSize), textLineHeightH)
//      rankCtx.fillRect(rightBegin.toInt, (2 * index + 1 + currentRankBaseLine) * textLineHeightH,
//        0 + killUnit * score.k, textLineHeightH)
      rankCtx.fillRect((2 * index + currentRankBaseLine) * textLineHeightH,rightBegin,
        textLineHeightH,5 + areaUnit * (score.area.toDouble / canvasSize))
      rankCtx.fillRect((2 * index + 1 + currentRankBaseLine) * textLineHeightH,rightBegin,
        textLineHeightH,0 + killUnit * score.k)
      rankCtx.restore()

//      rankCtx.setGlobalAlpha(1)
//      rankCtx.setFill(Color.rgb(0,0,0))
      index += 1
//      drawTextLine(s"[$index]: ${score.n.+("   ").take(3)}", rightBegin.toInt, index, currentRankBaseLine)
//      drawTextLine(f"${score.area.toDouble / canvasSize * 100}%.2f" + s"%", rightBegin.toInt + 60, index, currentRankBaseLine)
//      drawTextLine(s"kill=${score.k}", rightBegin.toInt + 160, index, currentRankBaseLine)
    }
  }

  def drawClearRank(): Unit = {
    val width = rankCanvas.getWidth
    val height = rankCanvas.getHeight
    rankCtx.clearRect(0, 0, width, height)
  }

  def drawTextLine(str: String, x: Int, lineNum: Int, lineBegin: Int = 0): Unit = {
    humanViewCtx.fillText(str, x, (lineNum + lineBegin - 1) * textLineHeight)
  }

  def setScale(scale: Double, x: Double, y: Double): Unit = {
    selfViewCtx.translate(x, y)
    selfViewCtx.scale(scale, scale)
    selfViewCtx.translate(-x, -y)
  }

  def isPointInWindow(p: Point4Trans, windowMax: Point, windowMin: Point): Boolean = {
    p.y < windowMax.y && p.y > windowMin.y && p.x > windowMin.x && p.x < windowMax.x
  }

  def findOffset(canvas: Canvas,uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long,
                 grid: Grid, frameRate: Int, isHuman: Boolean = false) = {
    val ctx = canvas.getGraphicsContext2D
    val snakes = data.snakes
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

    if (!isHuman) {
      val w = windowBoundary.x
      val h = windowBoundary.y
      ctx.clearRect(0, 0, w, h)
      ctx.setFill(Color.BLACK)
      ctx.save()
      ctx.fillRect(0, 0, w, h)
      ctx.restore()
    }
    else {
      humanViewCtx.clearRect(0, 0, humanWindowBoundary.x, humanWindowBoundary.y)
      humanViewCtx.setFill(ColorsSetting.backgroundColor)
      humanViewCtx.fillRect(0,0,humanWindowBoundary.x,humanWindowBoundary.y)
    }

    val snakeWithOff = data.snakes.map(i => i.copy(header = Point(i.header.x + offx, y = i.header.y + offy)))
    //    val fieldInWindow = data.fieldDetails.map { f => FieldByColumn(f.uid, f.scanField.filter(p => p.y < maxPoint.y && p.y > minPoint.y)) }
    var fieldInWindow: List[FrontProtocol.Field4Draw] = Nil
    data.fieldDetails.foreach { user =>
      if (snakes.exists(_.id == user.uid)) {
        var userScanField: List[FrontProtocol.Scan4Draw] = Nil
        user.scanField.foreach { field =>
          if (field.x < maxPoint.x && field.x > minPoint.x) {
            userScanField = FrontProtocol.Scan4Draw(field.x, field.y.filter(y => y._1 < maxPoint.y || y._2 > minPoint.y)) :: userScanField
          }
        }
        fieldInWindow = FrontProtocol.Field4Draw(user.uid, userScanField) :: fieldInWindow
      }
    }


    val bodyInWindow = data.bodyDetails.filter { b =>
      b.turn.exists(p => isPointInWindow(p, maxPoint, minPoint)) && snakes.exists(_.id == b.uid)
    }

    (snakeWithOff, fieldInWindow, bodyInWindow, offx, offy)
  }

  def drawForBody(bodyInWindow: List[FrontProtocol.BodyInfo4Draw], canvas: Canvas, snakes: List[SkDt], offx: Float, offy: Float, canvasUnit: Double = canvasUnit ): Unit = {
    val ctx = canvas.getGraphicsContext2D
    ctx.setGlobalAlpha(0.6)
    bodyInWindow.foreach { bds =>
      val color = snakes.find(_.id == bds.uid).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
      ctx.setFill(color)
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
      if (turnPoints.nonEmpty) {
        ctx.fillRect((turnPoints.last.x + offx) * canvasUnit, (turnPoints.last.y + offy) * canvasUnit, canvasUnit, canvasUnit)
      }
    }
  }

  def drawForField(fieldInWindow: List[FrontProtocol.Field4Draw], canvas: Canvas, snakes: List[SkDt], offx: Float, offy: Float, canvasUnit: Double = canvasUnit): Unit = {
    val ctx = canvas.getGraphicsContext2D
    ctx.setGlobalAlpha(1)
    fieldInWindow.foreach { field => //按行渲染
      val color = snakes.find(_.id == field.uid).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
      val darkC = findDarkColor(color)
      field.scanField.foreach { point =>
        point.y.foreach { y =>
          ctx.setFill(color)
          ctx.fillRect((point.x + offx) * canvasUnit, (y._1 + offy) * canvasUnit, canvasUnit * 1.05, canvasUnit * (y._2 - y._1 + 1))
          ctx.setFill(Constant.hex2Rgb(darkC))
          ctx.fillRect((point.x + offx) * canvasUnit, (y._2 + offy + 0.7) * canvasUnit, canvasUnit * 1.03, canvasUnit * 0.3)
        }
      }
    }
//    ctx.setGlobalAlpha(1)
//    fieldInWindow.foreach { field => //按行渲染
//      val color = snakes.find(_.id == field.uid).map(s => Constant.hex2Rgb(s.color)).getOrElse(ColorsSetting.defaultColor)
//      ctx.setFill(color)
//      field.scanField.foreach { fids =>
//        fids.y.foreach{y =>
//          fids.x.foreach{x =>
//            ctx.fillRect((x._1 + offx) * canvasUnit, (y._1 + offy) * canvasUnit, canvasUnit * (x._2 - x._1 + 1), canvasUnit * (y._2 - y._1 + 1.05))
//          }
//        }
//      }
//    }
  }

  def drawForHeader(snakeWithOff: List[SkDt], canvas: Canvas, uid: String, offsetTime: Long, grid: Grid, frameRate: Int, championId: String, canvasUnit: Double = canvasUnit): Unit ={
    val ctx = canvas.getGraphicsContext2D
    ctx.setGlobalAlpha(1)
    snakeWithOff.foreach { s =>
      

      val nextDirection = grid.nextDirection(s.id).getOrElse(s.direction)
      val direction = if (s.direction + nextDirection != Point(0, 0)) nextDirection else s.direction
      val off = direction * offsetTime.toFloat / frameRate
      
      val color = Constant.hex2Rgb(s.color)
      ctx.setFill(color)
      ctx.fillRect((s.header.x + off.x) * canvasUnit, (s.header.y + off.y) * canvasUnit, canvasUnit, canvasUnit)

      //      val otherHeaderImg = dom.document.getElementById(imgMap(s.img)).asInstanceOf[Image]
      //      val img = if (s.id == uid) myHeaderImg else otherHeaderImg

      if (s.id == championId)
        ctx.drawImage(championHeaderImg, (s.header.x + off.x) * canvasUnit, (s.header.y + off.y - 1 - 0.3) * canvasUnit, canvasUnit, canvasUnit) //头部图片绘制在名字上方
      //      ctx.drawImage(img, (s.header.x + off.x) * canvasUnit, (s.header.y + off.y) * canvasUnit, canvasUnit, canvasUnit) //头部图片绘制在名字上方

      val lightC = findLightColor(s.color)
      val darkC = findDarkColor(s.color)
      //      println(s"color-${s.color},lightC-$lightC,darkC-$darkC")
      ctx.setFill(Constant.hex2Rgb(lightC))
      ctx.fillRect((s.header.x + off.x) * canvasUnit, (s.header.y + off.y - 0.3) * canvasUnit, canvasUnit, canvasUnit)
      ctx.setFill(Constant.hex2Rgb(darkC))
      ctx.fillRect((s.header.x + off.x) * canvasUnit, (s.header.y + off.y + 1 - 0.3) * canvasUnit, canvasUnit, 0.3 * canvasUnit)

//      ctx.setFill(Constant.hex2Rgb(s.color))
//
//      val nextDirection = grid.nextDirection(s.id).getOrElse(s.direction)
//      val direction = if (s.direction + nextDirection != Point(0, 0)) nextDirection else s.direction
//      val off = direction * offsetTime.toFloat / frameRate
//      ctx.fillRect((s.header.x + off.x) * canvasUnit, (s.header.y + off.y) * canvasUnit, canvasUnit, canvasUnit)
//
//      val otherHeaderImg = imgMap(s.img)
//      val img = if (s.id == uid) myHeaderImg else otherHeaderImg
//      ctx.drawImage(img, (s.header.x + off.x) * canvasUnit, (s.header.y + off.y) * canvasUnit, canvasUnit, canvasUnit)

      //      ctx.setFont(Font.font(16))
      //      ctx.setFill(Color.rgb(0, 0, 0))
      //      val t = new Text(s"${s.name}")
      //      ctx.fillText(s.name, (s.header.x + off.x) * canvasUnit + canvasUnit / 2 - t.getLayoutBounds.getWidth / 2, (s.header.y + off.y - 1) * canvasUnit - 3)
    }
    ctx.restore()
  }

  def findLightColor(str: String) = {
    val (r, g, b) = hex2Rgb(str)
    val newR = decToHex(r+20)
    val newG = decToHex(g+20)
    val newB = decToHex(b+20)
    s"#$newR$newG$newB".toUpperCase
  }

  def findDarkColor(str: String) = {
    val (r, g, b) = hex2Rgb(str)
    val newR = decToHex(r-20)
    val newG = decToHex(g-20)
    val newB = decToHex(b-20)
    s"#$newR$newG$newB".toUpperCase
  }

  def findDarkColor(color: Color) = {
    val (r, g, b) = (color.getRed * 255,color.getGreen * 255,color.getBlue * 255)
    val newR = decToHex(r.toInt-20)
    val newG = decToHex(g.toInt-20)
    val newB = decToHex(b.toInt-20)
    s"#$newR$newG$newB".toUpperCase
  }

  def hex2Rgb(hex: String):(Int, Int, Int) = {
    val red = Constant.hexToDec(hex.slice(1, 3))
    val green = Constant.hexToDec(hex.slice(3, 5))
    val blue = Constant.hexToDec(hex.takeRight(2))
    (red, green, blue)
  }


  def decToHex(num: Int) = {
    Integer.toHexString(num)
  }

}
