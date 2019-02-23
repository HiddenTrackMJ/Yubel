package org.seekloud.yubel.paperClient

import java.awt.event.KeyEvent
import org.seekloud.yubel.paperClient.Protocol._
import scala.util.Random
import scala.collection.mutable

/**
  * User: Taoz
  * Date: 9/1/2016
  * Time: 5:34 PM
  */
trait Grid {

  val boundary: Point

  def debug(msg: String): Unit

  def info(msg: String): Unit

  val random = new Random(System.nanoTime())

  val maxDelayed = 6 //最大接收10帧以内的延时
  val historyRankLength = 5
  var frameCount = 0
  var grid: Map[Point, Spot] = Map[Point, Spot]()
  var snakes = Map.empty[String, SkDt]
  var actionMap = Map.empty[Int, Map[String, Int]] //Map[frameCount,Map[id, keyCode]]
  var killHistory = Map.empty[String, (String, String, Int)] //killedId, (killerId, killerName,frameCount)
  var snakeTurnPoints = Map.empty[String, List[Point4Trans]] //保留拐点
  var mayBeDieSnake = Map.empty[String, String] //可能死亡的蛇 killedId,killerId
  var mayBeSuccess = Map.empty[String, Map[Point, Spot]] //圈地成功后的被圈点 userId,points
  var hsMap = Map.empty[Int, (Map[String, SkDt], Map[Point, Spot], Map[String, List[Point4Trans]])] //保留近期的状态以方便回溯 (frame, (snake, pointd, turnPoints))
  var historyFieldInfo = Map.empty[Int, List[FieldByColumn]] //回溯
  var historyNewSnake = Map.empty[Int, (List[SkDt], List[FieldByColumn])] //回溯
  var historyDieSnake = Map.empty[Int, List[String]] //回溯
  var brickMap = Map.empty[Point,Brick]
  var boardMap = Map.empty[String,Board]
  var ballMap = Map.empty[String,Ball]
  var brickSideMap = Map.empty[(Point,Point),Brick]
  var scoreMap = Map.empty[String,Score]
  var historyStateMap = Map.empty[Int, AllData]
  var boardActionMap = Map.empty[Int, Map[String, (Int,Int)]]
  var colors = List.empty[String]
  var historyDieBoard = Map.empty[Int, List[String]]
  var historyDead = Map.empty[Int, (Long, Long)]

  var boundaryMap = List(Border(1,Point(0,0),BorderSize.w,1),Border(2,Point(0,BorderSize.h - 1),BorderSize.w,1),
    Border(3,Point(0,0),1,BorderSize.h),Border(4,Point(BorderSize.w - 1,0),1,BorderSize.h))
  List(0, BorderSize.w).foreach(x => (0 until BorderSize.h).foreach(y => grid += Point(x, y) -> Border))
  List(0, BorderSize.h).foreach(y => (0 until BorderSize.w).foreach(x => grid += Point(x, y) -> Border))

  def removeSnake(id: String): Option[SkDt] = {
    val r = snakes.get(id)
    if (r.isDefined) {
      snakes -= id
    }
    r
  }

  def addAction(id: String, keyCode: Int): Unit = {
    addActionWithFrame(id, keyCode, frameCount)
  }

  def addActionWithFrame(id: String, keyCode: Int, frame: Int): Unit = {
//    val map = actionMap.getOrElse(frame, Map.empty)
//    val tmp = map + (id -> keyCode)
//    actionMap += (frame -> tmp)
    val map1 = boardActionMap.getOrElse(frame, Map.empty)
    val tmp1 = map1 + (id -> (keyCode,0))
    boardActionMap += (frame -> tmp1)
    println("action size: " + boardActionMap.size)
//    val map1 = boardActionMap.getOrElse(frame, Map.empty)
//    val tmp1 = map1 + (id -> keyCode)
//    boardActionMap += (frame -> tmp1)
  }

  def addActionWithFrame(id: String, keyCode: Int, frame: Int, typ: Int): Unit = {
    val map1 = boardActionMap.getOrElse(frame, Map.empty)
    val tmp1 = map1 + (id -> (keyCode,typ))
    boardActionMap += (frame -> tmp1)
//    println("action size: " + boardActionMap.size)
  }

  def getUserMaxActionFrame(id: String, frontFrame: Int): (Int, Int) = {
    val existFrame = boardActionMap.map { a =>
      (a._1, a._2.filter(_._1 == id)) }.filter(_._2.nonEmpty)
    try {
      (Math.max(existFrame.keys.max + 1, frontFrame), existFrame(existFrame.keys.max)(id)._1)
    } catch {
      case e: Exception =>
        (frontFrame, 0)
    }
  }

  def checkActionFrame(id: String, frontFrame: Int): Int = {
    val backendFrame = Math.max(frontFrame, frameCount)
    val existFrame = boardActionMap.map { a => (a._1, a._2.filter(_._1 == id)) }.filter(_._2.nonEmpty).keys
    try {
      Math.max(existFrame.max + 1, backendFrame)
    } catch {
      case e: Exception =>
        backendFrame
    }
  }

  def deleteActionWithFrame(id: String, frame: Int): Unit = {
    println("deleting")
    val map = boardActionMap.getOrElse(frame, Map.empty)
    val tmp = map - id
    boardActionMap += (frame -> tmp)
  }

  def nextDirection(id: String): Option[Point] = {
    val map = actionMap.getOrElse(frameCount, Map.empty)
    map.get(id) match {
      case Some(KeyEvent.VK_LEFT) => Some(Point(-1, 0))
      case Some(KeyEvent.VK_RIGHT) => Some(Point(1, 0))
      case Some(KeyEvent.VK_UP) => Some(Point(0, -1))
      case Some(KeyEvent.VK_DOWN) => Some(Point(0, 1))
      case _ => None
    }
  }

  def update(origin: String): Unit = {
    updateSpots()
    updateBoards()
    updateBalls()
    val limitFrameCount = frameCount - (maxDelayed + 1)
    actionMap = actionMap.filter(_._1 > limitFrameCount)
    historyFieldInfo = historyFieldInfo.filter(_._1 > limitFrameCount)
    hsMap = hsMap.filter(_._1 > limitFrameCount)
    historyNewSnake = historyNewSnake.filter(_._1 > limitFrameCount)
    historyDieSnake = historyDieSnake.filter(_._1 > limitFrameCount)
    frameCount += 1
  }

  def updateSpots(): Unit = {
    grid = grid.filter { case (p, spot) =>
      spot match {
        case Body(id, _) if snakes.contains(id) => true
        case Field(id) if snakes.contains(id) => true
        case Border => true
        case _ => false
      }
    }
  }

  def getLevel(): (List[(Int, Point)],Int)= {
    val w = ((0.8 * BorderSize.w) / brickWidth).toInt
    var h = ((0.4 * BorderSize.h) / brickHeight).toInt
    if (h > 6) h = 6
    val brickList = (0 until  w ).toList.flatMap( x =>
      (0 until h).toList.map( y =>
        (y + 1 , Point((0.1 * BorderSize.w).toInt + x * brickWidth, (0.1 * BorderSize.h).toInt + brickHeight * y) )
      )
    )
    (brickList,h)
  }

  def getBrickSides(): Unit = {
    var sideMap = Map.empty[(Point,Point),Brick]
    brickMap.foreach{ brick =>
      val p = brick._1
      val sides = List((p, p + Point(brickWidth,0)),(p, p + Point(0,brickHeight)),
        (p + Point(0,brickHeight), p + Point(brickWidth,brickHeight)),
        (p + Point(brickWidth,0),p + Point(brickWidth,brickHeight))
      )
      sides.foreach{s =>
        if (sideMap.contains(s)) sideMap = sideMap - s
        else sideMap += (s -> brick._2)
      }
    }
    if (sideMap.nonEmpty){
      brickSideMap = sideMap
    }
    println("side " + brickSideMap)
  }

  def isMiddle(a: Float, b: Float, c: Float): Boolean = {
    if((c >= a && c <= b) || (c >= b && c <= a)) true
    else false
  }

  def touchedBrick(c: Point, nc: Point): List[(Brick, String)] = {
    var collision: List[(Brick, String)] = List.empty
    brickMap.foreach{ b =>
      var flag = ""
      if (b._1.x <= nc.x && b._1.x + brickWidth >= nc.x
        && b._1.y <= nc.y && b._1.y + brickHeight >= nc.y) {
        if (nc.x != c.x){
          val gradient = (nc.y - c.y) / (nc.x - c.x)
          def xAxis(y: Float): Float = {
            (y + gradient * c.x - c.y) / gradient
          }
          def yAxis(x: Float): Float = {
            gradient * x + c.y - gradient * c.x
          }
          List(b._1.x, b._1.x + brickWidth).foreach{ x =>
            val y = yAxis(x)
            if (isMiddle(c.y, nc.y, y) && isMiddle(b._1.y, b._1.y + brickHeight, y)) flag = "x"
//            println("x" + x,(c.y, nc.y, y),(b._1.y, b._1.y + brickHeight, y))
          }
          List(b._1.y, b._1.y + brickHeight).foreach{ y =>
            val x = xAxis(y)
            if (isMiddle(c.x, nc.x, x) && isMiddle(b._1.x, b._1.x + brickWidth, x)) flag = "y"
//            println("y" + y,(c.x, nc.x, x),(b._1.x, b._1.x + brickWidth, x))
          }
//          println("gradient: "+ gradient + "flag: " + flag)
        }
        else {
          flag = "y"
        }
        collision = collision :+ (b._2, flag)
      }
    }
//    println("gradient: "+ gradient + "collision: " + collision)
    collision
  }

  def touchedBoard(c: Point, nc: Point): List[(Board, String)] = {
    var collision: List[(Board, String)] = List.empty
    boardMap.foreach{ b =>
      var flag = ""
      if (b._2.center.x - getBoardWidth / 2 <= nc.x && b._2.center.x + getBoardWidth / 2 >= nc.x
        && b._2.center.y <= nc.y && b._2.center.y + boardHeight >= nc.y) {
        if (nc.x != c.x){
          val gradient = (nc.y - c.y) / (nc.x - c.x)
          def xAxis(y: Float): Float = {
            (y + gradient * c.x - c.y) / gradient
          }
          def yAxis(x: Float): Float = {
            gradient * x + c.y - gradient * c.x
          }
          List(b._2.center.x - getBoardWidth / 2, b._2.center.x + getBoardWidth / 2).foreach{ x =>
            val y = yAxis(x)
            if (isMiddle(c.y, nc.y, y) && isMiddle(b._2.center.y, b._2.center.y + boardHeight, y)) flag = "x"
//                        println("x" + x,(c.y, nc.y, y),(b._1.y, b._1.y + brickHeight, y))
          }
          List(b._2.center.y, b._2.center.y + boardHeight).foreach{ y =>
            val x = xAxis(y)
            if (isMiddle(c.x, nc.x, x) && isMiddle(b._2.center.x - getBoardWidth / 2, b._2.center.x + getBoardWidth / 2, x)) flag = "y"
            //            println("y" + y,(c.x, nc.x, x),(b._1.x, b._1.x + brickWidth, x))
          }
//                    println("gradient: "+ gradient + "flag: " + flag)
        }
        else {
          flag = "y"
//          println("y: " + flag)
        }
        collision = collision :+ (b._2, flag)
      }
    }
//    println("collision: " + collision)
    collision
  }

  def touchedBoundary(c: Point, nc: Point): List[(Border, String)] = {
    var collision: List[(Border, String)] = List.empty
    boundaryMap.foreach{ b =>
      var flag = ""
      if (b.id == 1) {
        if (b.center.x <= nc.x && b.center.x + b.width >= nc.x
          && b.center.y + b.height  >= nc.y && b.center.y <= nc.y) {
          flag = "y"
          collision = collision :+ (b, flag)
        }
      }
      if (b.id == 2) {
        if (b.center.x <= nc.x && b.center.x + b.width >= nc.x
          && b.center.y <= nc.y  && b.center.y + b.height  >= nc.y) {
          flag = "y"
          collision = collision :+ (b, flag)
        }
      }
      else if(b.id == 3) {
        if (b.center.y <= nc.y && b.center.y + b.height >= nc.y
          && b.center.x + b.width >= nc.x  && b.center.x <= nc.x ) {
          flag = "x"
          collision = collision :+ (b, flag)
        }
      }
      else if(b.id == 4) {
        if (b.center.y <= nc.y && b.center.y + b.height >= nc.y
          && b.center.x <= nc.x && b.center.x + b.width >= nc.x) {
          flag = "x"
          collision = collision :+ (b, flag)
        }
      }
    }
    //    println("collision: " + collision)
    collision
  }

  def updateBoards():Unit = {
    //    val acts = actionMap.getOrElse(frameCount, Map.empty[String, Int])
    val acts = boardActionMap.getOrElse(frameCount, Map.empty[String, (Int, Int)])
    boardMap = boardMap.map { board =>
      val keyCode = acts.get(board._2.id)
      var center =  board._2.center + board._2.direction * 2
      var keyDirection = board._2.direction
      var emotion = board._2.emotion
      if (keyCode.isDefined){
//        println("key :" + keyCode)

        keyCode.get._1 match {
          case KeyEvent.VK_LEFT => keyDirection = Point(-1, 0)
          case KeyEvent.VK_RIGHT => keyDirection = Point(1, 0)
          case _ =>
        }
        keyCode.get._2 match  {
          //          case 0 => keyDirection =  keyDirection
          case 1 =>
            keyDirection = Point(0,0)
            if (keyCode.get._1 == KeyEvent.VK_Q || keyCode.get._1 == KeyEvent.VK_W ||
              keyCode.get._1 == KeyEvent.VK_E || keyCode.get._1 == KeyEvent.VK_R)
              emotion = 0
          case _ =>
            keyCode.get._1 match {
              case KeyEvent.VK_Q => emotion = 1
              case KeyEvent.VK_W => emotion = 2
              case KeyEvent.VK_E => emotion = 3
              case KeyEvent.VK_R => emotion = 4
              case _ =>
            }
        }
      }

      val c = if (keyDirection == Point(1,0)) board._2.center + Point(getBoardWidth / 2,0)
      else board._2.center - Point(getBoardWidth / 2,0)
      touchedBoundary(c,c + keyDirection) match {
        case b: List[(Border, String)] =>
          var flag = true
          b.foreach{ b =>
            b._2 match {
              case "x" if flag =>
                keyDirection = Point(0, 0)
                center = board._2.center
                flag = false
              case _ =>
            }
          }
        //          case Nil =>
        case _   =>
      }

      board._1 -> Board(board._2.id,board._2.color,board._2.name,
        center, keyDirection, board._2.length, emotion, board._2.lengthTime, board._2.yubelId)
    }
  }

  def updateBalls():Unit = {
    //    println("bricks: " + brickMap.size)
    //    val acts = actionMap.getOrElse(frameCount, Map.empty[String, Int])
    val acts = boardActionMap.getOrElse(frameCount, Map.empty[String, (Int, Int)])
    var deadList  = List.empty[String]
    ballMap = ballMap.map { ball =>
      if (ball._2.center.y > 0.95 * BorderSize.h) deadList = deadList :+ ball._1
      val keyCode = acts.get(ball._2.id)
      var center =  ball._2.center + ball._2.direction
      var keyDirection = Point(0,0)
      var move = ball._2.moveOrNot
      if (!move) {
        center =  ball._2.center + ball._2.direction * 2
        keyDirection = ball._2.direction
        if (keyCode.isDefined){
          keyCode.get._1 match {
            case KeyEvent.VK_UP =>
              move = true
              keyDirection = ball._2.direction + Point(0, -1)
              keyDirection =
                keyDirection / Math.sqrt(Math.pow(keyDirection.x, 2) + Math.pow(keyDirection.y, 2)).toFloat
            case KeyEvent.VK_LEFT => keyDirection = Point(-1, 0)
            case KeyEvent.VK_RIGHT => keyDirection = Point(1, 0)
            case _ =>
          }
          keyCode.get._2 match  {
            //          case 0 => keyDirection =  keyDirection
            case 1 => keyDirection = Point(0,0)
            case _ =>
          }
        }
        val c = if (keyDirection == Point(1,0)) ball._2.center + Point(getBoardWidth / 2,0)
        else ball._2.center - Point(getBoardWidth / 2,0)
        touchedBoundary(c,c + keyDirection) match {
          case b: List[(Border, String)] =>
            var flag = true
            b.foreach{ b =>
              b._2 match {
                case "x" if flag =>
                  keyDirection = Point(0, 0)
                  center = ball._2.center
                  flag = false
                case _ =>
              }
            }
          //          case Nil =>
          case _   =>
        }
      }
      else {
        keyDirection = ball._2.direction
        val nextCenter = ball._2.center +
          keyDirection
        val c = ball._2.center
        touchedBrick(c, nextCenter) match {
          case brick: List[(Brick, String)] =>
            var flag = true
            brick.foreach{ b =>
              val hp = b._1.hp - 1
              if (hp == 0) brickMap -= b._1.center
              else {
                val oldBrick = brickMap.get(b._1.center)
                if (oldBrick.isDefined) {
                  val old = oldBrick.get
                  val newBrick = Brick(old.bid,old.bonus,old.color, hp,old.center)
                  brickMap -= b._1.center
                  brickMap += (b._1.center -> newBrick)
                }
              }
              val oldScore = scoreMap.get(ball._1)
              if (oldScore.isDefined) {
                val newScore = oldScore.get.score + scorePerBrick
                scoreMap = scoreMap.map( s =>
                  s._1 -> {
                    if (s._1 != ball._1) s._2 else Score(s._1, s._2.name, s._2.color, newScore)}
                )
              }
              b._2 match {
                case "x" if flag =>
                  keyDirection = Point(-keyDirection.x, keyDirection.y)
                  flag = false
                case "y" if flag =>
                  keyDirection = Point(keyDirection.x, -keyDirection.y)
                  flag = false
                case _ =>
              }
            }
          //          case Nil =>
          case _   =>
        }
        touchedBoard(c, nextCenter ) match {
          case board: List[(Board, String)] =>
            //            println("board: " + board)
            var flag = true
            board.foreach{ b =>
              b._2 match {
                case "x" if flag =>
                  keyDirection = Point(-keyDirection.x, keyDirection.y)
                  flag = false
                case "y" if flag =>
                  keyDirection = Point(keyDirection.x, -keyDirection.y)
                  flag = false
                case _ =>
              }
              keyDirection = keyDirection + b._1.direction
              keyDirection =
                keyDirection  / math.sqrt(math.pow(keyDirection.x, 2) + math.pow(keyDirection.y, 2)).toFloat * 1.5.toFloat
            }
          //          case Nil =>
          case _   =>
        }
        touchedBoundary(c, c + keyDirection / Math.sqrt(Math.pow(keyDirection.x, 2) + Math.pow(keyDirection.y, 2)).toFloat) match {
          case border: List[(Border, String)] =>
            var flag = true
            border.foreach{ b =>
              b._2 match {
                case "x" if flag =>
                  keyDirection = Point(-keyDirection.x, keyDirection.y)
                  flag = false
                case "y" if flag =>
                  keyDirection = Point(keyDirection.x, -keyDirection.y)
                  flag = false
                case _ =>
              }
            }
            if (border.length > 1) keyDirection = keyDirection * -1
          //          case Nil =>
          case _   =>
        }
      }
      //      println(ball._2.id,keyDirection)
      ball._1 -> Ball(ball._2.id,ball._2.color,ball._2.name,
        center, keyDirection, move, ball._2.yubelId)
    }
    historyDieBoard += (frameCount -> deadList)
  }

  def randomEmptyPoint(size: Int): Point = {
    var p = Point(random.nextInt(boundary.x.toInt - size), random.nextInt(boundary.y.toInt - size))
    while ((0 until size * 2).flatMap { x =>
      (0 until size * 2).map { y =>
        grid.contains(p.copy(x = p.x + x, y = p.y + y))
      }
    }.contains(true)) {
      p = Point(random.nextInt(boundary.x.toInt - size), random.nextInt(boundary.y.toInt - size))
    }
    p + Point(2 + random.nextInt(2), 2 + random.nextInt(2))
  }




  def getAllData: AllData={
    AllData(frameCount,brickMap,boardMap,ballMap)
  }

  def addAllData(all : AllData):Unit = {
    frameCount =all.frameCount
    brickMap = all.bricks
    boardMap = all.boards
    ballMap = all.balls
  }

  def addData(all : DataExceptBrick):Unit = {
    frameCount = all.frameCount
    boardMap ++= all.boards
    ballMap ++= all.balls
    scoreMap ++= all.score
  }

//  def addDataExceptBall(data : DataExceptBall): Unit = {
//    brickMap = data.bricks
//    boardMap = data.boards
//  }

  def getKiller(myId: String): Option[(String, String, Int)] = {
    killHistory.get(myId) match {
      case Some(info) if info._3 > frameCount - 3 => Some(info)
      case _ => None
    }
  }

  def getPointBelong(id: String, point: Point): Boolean = {
    grid.get(point) match {
      case Some(Field(fid)) if fid == id => true
      case _ => false
    }
  }

  def cleanData(): Unit = {
    brickMap = Map.empty[Point,Brick]
    boardMap = Map.empty[String,Board]
    ballMap = Map.empty[String,Ball]
    brickSideMap = Map.empty[(Point,Point),Brick]
    scoreMap = Map.empty[String,Score]
    boardActionMap = Map.empty[Int, Map[String, (Int,Int)]]

    snakes = Map.empty[String, SkDt]
    actionMap = Map.empty[Int, Map[String, Int]]
    grid = grid.filter(_._2 match { case Border => true case _ => false })
    killHistory = Map.empty[String, (String, String, Int)]
    snakeTurnPoints = snakeTurnPoints.empty
  }

  def returnBackField(snakeId: String): Unit = { //归还身体部分所占有的领地
    snakeTurnPoints -= snakeId
    val bodyGrid = grid.filter(_._2 match { case Body(bid, _) if bid == snakeId => true case _ => false })
    bodyGrid.foreach {
      case (p, Body(_, fid)) if fid.nonEmpty => grid += p -> Field(fid.get)
      case (p, _) => grid -= p
    }
  }



  def cleanSnakeTurnPoint(sid: String):Unit = {
    if (snakeTurnPoints.contains(sid)) {
      snakeTurnPoints -= sid
    }
  }

  def cleanDiedBoardInfo(dieBoard: List[String]): Unit = {
    dieBoard.foreach{ d =>
      boardMap -= d
      ballMap -= d
      scoreMap -= d
    }
  }

}

