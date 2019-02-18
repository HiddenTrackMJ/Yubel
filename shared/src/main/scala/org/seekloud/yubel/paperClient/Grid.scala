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

  val maxDelayed = 11 //最大接收10帧以内的延时
  val historyRankLength = 5
  var frameCount = 0
  var grid: Map[Point, Spot] = Map[Point, Spot]()
  var snakes = Map.empty[String, SkDt]
  var actionMap = Map.empty[Int, Map[String, Int]] //Map[frameCount,Map[id, keyCode]]
  var killHistory = Map.empty[String, (String, String, Int)] //killedId, (killerId, killerName,frameCount)
  var snakeTurnPoints = Map.empty[String, List[Point4Trans]] //保留拐点
  var mayBeDieSnake = Map.empty[String, String] //可能死亡的蛇 killedId,killerId
  var mayBeSuccess = Map.empty[String, Map[Point, Spot]] //圈地成功后的被圈点 userId,points
  var historyStateMap = Map.empty[Int, (Map[String, SkDt], Map[Point, Spot], Map[String, List[Point4Trans]])] //保留近期的状态以方便回溯 (frame, (snake, pointd, turnPoints))
  var historyFieldInfo = Map.empty[Int, List[FieldByColumn]] //回溯
  var historyNewSnake = Map.empty[Int, (List[SkDt], List[FieldByColumn])] //回溯
  var historyDieSnake = Map.empty[Int, List[String]] //回溯
  var brickMap = Map.empty[Point,Brick]
  var boardMap = Map.empty[String,Board]
  var ballMap = Map.empty[String,Ball]
  var brickSideMap = Map.empty[(Point,Point),Brick]
  var boardActionMap = Map.empty[Int, Map[String, (Int,Int)]]
  var colors = List.empty[String]
  var historyDieBoard = Map.empty[Int, List[String]]

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

  def update(origin: String): (List[(String, List[Point])], List[String]) = {
    val isFinish = updateSnakes(origin)
    updateSpots()
    updateBoards()
    updateBalls()
    val limitFrameCount = frameCount - (maxDelayed + 1)
    actionMap = actionMap.filter(_._1 > limitFrameCount)
    historyFieldInfo = historyFieldInfo.filter(_._1 > limitFrameCount)
    historyStateMap = historyStateMap.filter(_._1 > limitFrameCount)
    historyNewSnake = historyNewSnake.filter(_._1 > limitFrameCount)
    historyDieSnake = historyDieSnake.filter(_._1 > limitFrameCount)
    frameCount += 1
    isFinish
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
    val h = ((0.4 * BorderSize.h) / brickHeight).toInt
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
//    brickSideMap.foreach{ s =>
//      if (s._1._1.x == s._1._2.x) {
//        if (nc.x == s._1._1.x && isMiddle(s._1._1.y, s._1._2.y, nc.y))
//          collision = Some(s._2,s._1)
//      }
//      else {
//        if (nc.y == s._1._1.y && isMiddle(s._1._1.x, s._1._2.x, nc.x))
//          collision = Some(s._2,s._1)
//      }
//    }
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
      if (b.id == 1 || b.id == 2) {
        if (b.center.x <= nc.x && b.center.x + b.width >= nc.x
          && b.center.y + b.height  >= nc.y && b.center.y  <= nc.y ) {
          flag = "y"
          collision = collision :+ (b, flag)
        }
      }
//      if (b.id == 2) {
//        if (b.center.x <= nc.x && b.center.x + b.width >= nc.x
//          && b.center.y <= nc.y  && b.center.y + b.height  >= nc.y) {
//          flag = "y"
//          collision = collision :+ (b, flag)
//        }
//      }
      else if(b.id == 3 || b.id == 4) {
        if (b.center.y <= nc.y && b.center.y + b.height >= nc.y
          && b.center.x + b.width >= nc.x  && b.center.x  <= nc.x) {
          flag = "x"
          collision = collision :+ (b, flag)
        }
      }
//      else if(b.id == 4) {
//        if (b.center.y <= nc.y && b.center.y + b.height >= nc.y
//          && b.center.x <= nc.x && b.center.x + b.width >= nc.x) {
//          flag = "x"
//          collision = collision :+ (b, flag)
//        }
//      }
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



      //      println(board_2.id,board._2.center)
      board._1 -> Board(board._2.id,board._2.color,board._2.name,
        center, keyDirection, emotion, board._2.carnieId)
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
        val nextCenter = ball._2.center + keyDirection
        val c = ball._2.center
        touchedBrick(c,nextCenter) match {
          case brick: List[(Brick, String)] =>
            var flag = true
            brick.foreach{ b =>
              brickMap -= b._1.center
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
            }
          //          case Nil =>
          case _   =>
        }
        touchedBoundary(c, nextCenter ) match {
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
        center, keyDirection, move, ball._2.carnieId)
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

  def updateSnakes(origin: String): (List[(String, List[Point])], List[String]) = {
    var finishFields = List.empty[(String, List[Point])]

    def updateASnake(snake: SkDt, actMap: Map[String, Int]): Either[String, UpdateSnakeInfo] = {
      val keyCode = actMap.get(snake.id)
      val newDirection = {
        val keyDirection = keyCode match {
          case Some(KeyEvent.VK_LEFT) => Point(-1, 0)
          case Some(KeyEvent.VK_RIGHT) => Point(1, 0)
          case Some(KeyEvent.VK_UP) => Point(0, -1)
          case Some(KeyEvent.VK_DOWN) => Point(0, 1)
          case _ => snake.direction
        }
        if (keyDirection + snake.direction != Point(0, 0)) {
          keyDirection
        } else {
          snake.direction
        }
      }

      if (newDirection != Point(0, 0)) {
        val newHeader = snake.header + newDirection
        grid.get(newHeader) match {
          case Some(x: Body) => //进行碰撞检测
            if (x.id != snake.id) { //撞到了别人的身体
              killHistory += x.id -> (snake.id, snake.name, frameCount)
            }
            mayBeDieSnake += x.id -> snake.id
            grid.get(snake.header) match { //当上一点是领地或被别人身体占用的领地时 记录出行的起点
              case Some(Field(fid)) if fid == snake.id && x.fid.getOrElse("") != snake.id =>
                snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header), x.fid))

              case Some(Body(bid, _)) if bid == snake.id && x.fid.getOrElse("") == snake.id =>
                enclosure(snake, origin, newHeader, newDirection)

              case _ =>
                if (snake.direction != newDirection)
                  snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(snake.header.x.toShort, snake.header.y.toShort))))
                Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), x.fid))
            }

          case Some(Field(id)) =>
            if (id == snake.id) {
              grid.get(snake.header) match {
                case Some(Body(bid, _)) if bid == snake.id => //回到了自己的领域
                  enclosure(snake, origin, newHeader, newDirection)

                case _ =>
                  Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), Some(id)))
              }
            } else { //进入到别人的领域
              grid.get(snake.header) match { //当上一点是领地或被别人身体占用的领地时 记录出行的起点
                case Some(Field(fid)) if fid == snake.id =>
                  snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                  Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header), Some(id)))

                case Some(Body(_, fid)) if fid.getOrElse("") == snake.id =>
                  snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                  Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header), Some(id)))

                case _ =>
                  if (snake.direction != newDirection)
                    snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(snake.header.x.toShort, snake.header.y.toShort))))
                  Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), Some(id)))
              }
            }

          case Some(Border) =>
            Left(snake.id)

          case _ =>
            grid.get(snake.header) match { //当上一点是领地或被别人身体占用的领地时 记录出行的起点
              case Some(Field(fid)) if fid == snake.id =>
                snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header)))

              case Some(Body(_, fid)) if fid.getOrElse("") == snake.id =>
                snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header)))

              case _ =>
                if (snake.direction != newDirection)
                  snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(snake.header.x.toShort, snake.header.y.toShort))))
                Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection)))
            }
        }
      }
      else Right(UpdateSnakeInfo(snake, Some(snake.id)))

    }

    var mapKillCounter = Map.empty[String, Int]
    var updatedSnakes = List.empty[UpdateSnakeInfo]
    var killedSnaked = List.empty[String]

    historyStateMap += frameCount -> (snakes, grid, snakeTurnPoints)

    val acts = actionMap.getOrElse(frameCount, Map.empty[String, Int])

    snakes.values.map(updateASnake(_, acts)).foreach {
      case Right(s) =>
        updatedSnakes ::= s

      case Left(sid) =>
        killedSnaked ::= sid
    }

    val intersection = mayBeSuccess.keySet.filter(p => mayBeDieSnake.keys.exists(_ == p))
    intersection.foreach { snakeId => // 在即将完成圈地的时候身体被撞击则不死但此次圈地作废
      mayBeDieSnake -= snakeId
      killHistory -= snakeId
      mayBeSuccess -= snakeId
    }

    mayBeSuccess.foreach { s =>
      s._2.foreach { p =>
        p._2 match {
          case Body(bodyId, _) if bodyId != s._1 => grid += p._1 -> Body(bodyId, Some(s._1))
          case _ => grid += p._1 -> Field(s._1)
        }
      }
    }

    //if two (or more) headers go to the same point
    val snakesInDanger = updatedSnakes.groupBy(_.data.header).filter(_._2.lengthCompare(1) > 0).flatMap { res =>
      val sids = res._2.map(_.data.id)
      grid.get(res._1) match {
        case Some(Field(fid)) if sids.contains(fid) =>
          sids.filterNot(_ == fid).foreach { killedId =>
            mayBeDieSnake += killedId -> fid
            killHistory += killedId -> (fid, snakes.get(fid).map(_.name).getOrElse(""), frameCount)
          }
          sids.filterNot(_ == fid)

        case _ =>
          sids.foreach{s =>
            sids.filterNot(_ == s).foreach{o =>
              mayBeDieSnake += s -> o
              killHistory += s -> (o, snakes.get(o).map(_.name).getOrElse(""), frameCount)
            }
          }
          sids
      }
    }.toList

    mayBeDieSnake.foreach { s =>
      mapKillCounter += s._2 -> (mapKillCounter.getOrElse(s._2, 0) + 1)
      killedSnaked ::= s._1
    }

    finishFields = mayBeSuccess.map(i => (i._1, i._2.keys.toList)).toList

    val finishPoints = finishFields.flatMap(_._2)

    val noHeaderSnake = snakes.filter(s => finishPoints.contains(updatedSnakes.find(_.data.id == s._2.id).getOrElse(UpdateSnakeInfo(SkDt((-1).toString, "", "", Point(0, 0), Point(-1, -1), yubelId = -1))).data.header)).keySet
    val bodyInNewFieldSnake = finishPoints.map { fp =>
      grid.get(fp) match {
        case Some(Body(bid, _)) => Some(bid)
        case _ => None
      }
    }.filter(_.nonEmpty).map(_.get)

    mayBeDieSnake = Map.empty[String, String]
    mayBeSuccess = Map.empty[String, Map[Point, Spot]]

    //    val noFieldSnake = snakes.keySet &~ grid.map(_._2 match { case Field(uid) => uid case _ => "" }).toSet.filter(_ != "") //若领地全被其它玩家圈走则死亡

    val finalDie = snakesInDanger ::: killedSnaked ::: noHeaderSnake.toList ::: bodyInNewFieldSnake

    finalDie.foreach { sid =>
      returnBackField(sid)
      grid ++= grid.filter(_._2 match { case Body(_, fid) if fid.nonEmpty && fid.get == sid => true case _ => false }).map { g =>
        Point(g._1.x, g._1.y) -> Body(g._2.asInstanceOf[Body].id, None)
      }
    }

    val newSnakes = updatedSnakes.filterNot(s => finalDie.contains(s.data.id)).map { s =>
      mapKillCounter.get(s.data.id) match {
        case Some(k) => s.copy(data = s.data.copy(kill = (k + s.data.kill).toShort))
        case None => s
      }
    }

    newSnakes.foreach { s =>
      if (s.bodyInField.nonEmpty && s.bodyInField.get == s.data.id) grid += s.data.header -> Field(s.data.id)
      else grid += s.data.header -> Body(s.data.id, s.bodyInField)
    }

    snakes = newSnakes.map(s => (s.data.id, s.data)).toMap

    (finishFields, finalDie.distinct)
  }

  def enclosure(snake: SkDt, origin: String, newHeader: Point, newDirection: Point) = {
    if (mayBeDieSnake.keys.exists(_ == snake.id)) { //如果在即将完成圈地的时候身体被撞击则不死但此次圈地作废
      killHistory -= snake.id
      mayBeDieSnake -= snake.id
      returnBackField(snake.id)
    } else {
      val stillStart = if (grid.get(snake.startPoint) match {
        case Some(Field(fid)) if fid == snake.id => true
        case _ => false
      }) true else false //起点是否被圈走
      if (stillStart && origin == "b") { //只在后台执行圈地算法
        snakeTurnPoints -= snake.id

        var finalFillPoll = grid.filter(_._2 match { case Body(bodyId, _) if bodyId == snake.id => true case _ => false })

        val myFieldPoint = grid.filter(_._2 match { case Field(fid) if fid == snake.id => true case _ => false }).keys.++(finalFillPoll.keys)

        val (xMin, xMax, yMin, yMax) = Short.findMyRectangle(myFieldPoint)

        var targets = Set.empty[Point] //所有需要检查的坐标值的集

        for (x <- xMin until xMax) {
          for (y <- yMin until yMax) {
            grid.get(Point(x, y)) match {
              case Some(x: Field) if x.id == snake.id => //donothing
              case Some(x: Body) if x.fid.nonEmpty && x.fid.get == snake.id =>
              case Some(x: Body) if x.id == snake.id =>
              case _ => targets = targets + Point(x, y)
            }
          }
        }

        while (targets.nonEmpty) {
          var iter = List.empty[Point]
          iter = iter :+ targets.head
          targets = targets.tail

          var fillPool = List.empty[Point] //该次填色需要上色的所有坐标
          var in_bound = true //这次上色是否为内部区域
          while (iter.nonEmpty) {
            val curr = iter.head
            iter = iter.tail
            Array(Point(-1, 0), Point(0, -1), Point(0, 1), Point(1, 0)).foreach { dir =>
              if (targets.contains(dir + curr)) { //如果 targets 包含该坐标，则将该坐标从targets中移除并添加至iter
                targets = targets - (dir + curr)
                iter = iter :+ (dir + curr)
              }
            }
            if (in_bound) {
              //如果curr紧邻field_border(boundary)，将in_bound设为False；否则向fill_pool中加入curr
              val aroundPoints = List(Point(-1, 0), Point(1, 0), Point(0, -1), Point(0, 1)).map(p => p + curr)
              if (aroundPoints.head.x <= xMin || aroundPoints(1).x >= xMax || aroundPoints(2).y <= yMin || aroundPoints(3).y >= yMax) {
                in_bound = false
              } else {
                fillPool ::= curr
              }
            }
          }
          if (in_bound) { //如果 in_bound 为真则将 fill_pool中所有坐标填充为当前玩家id
            for (p <- fillPool) {
              grid.get(p) match {
                case Some(Body(bodyId, originFid)) => finalFillPoll += p -> Body(bodyId, originFid)

                case Some(Border) => //doNothing

                case _ => finalFillPoll += p -> Blank
              }
            }
          }
        }
        mayBeSuccess += (snake.id -> finalFillPoll)
      } else returnBackField(snake.id)
    }
    Right(UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), Some(snake.id)))
  }

  def getGridData: Protocol.Data4TotalSync = {
    var fields: List[Fd] = Nil
    val bodyDetails = snakes.values.map { s => BodyBaseInfo(s.id, getSnakesTurn(s.id, s.header)) }.toList

    grid.foreach {
      case (p, Field(id)) => fields ::= Fd(id, p.x.toInt, p.y.toInt)
      case _ => //doNothing
    }

    val fieldDetails =
      fields.groupBy(_.id).map { case (userId, fieldPoints) =>
        FieldByColumn(userId, fieldPoints.groupBy(_.y).map { case (y, target) =>
          (y.toShort, Tool.findContinuous(target.map(_.x.toShort).sorted))
        }.toList.groupBy(_._2).map { case (r, target) =>
          ScanByColumn(Tool.findContinuous(target.map(_._1).sorted), r)
        }.toList)
      }.toList

    Protocol.Data4TotalSync(
      frameCount,
      snakes.values.toList,
      bodyDetails,
      fieldDetails
      //      killHistory.map(k => Kill(k._1, k._2._1, k._2._2, k._2._3)).toList
    )
  }

  def getAllData: allData={
    allData(brickMap,boardMap,ballMap)
  }

  def addAllData(all : allData):Unit = {
    brickMap = all.bricks
    boardMap = all.boards
    ballMap = all.balls
  }

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

  def getSnakesTurn(sid: String, header: Point): TurnInfo = {
    val turnPoint = snakeTurnPoints.getOrElse(sid, Nil)
    if (turnPoint.nonEmpty) {
      TurnInfo(turnPoint ::: List(Point4Trans(header.x.toShort, header.y.toShort)),
        grid.filter(_._2 match { case Body(id, fid) if id == sid && fid.nonEmpty => true case _ => false }).map(g =>
        (Point4Trans(g._1.x.toShort, g._1.y.toShort), g._2.asInstanceOf[Body].fid.get)).toList)
    } else TurnInfo(Nil, Nil)
  }

  def getMyFieldCount(uid: String, myFieldInfo: List[FrontProtocol.Scan4Draw]): Int = {
    var fieldCount = 0
    myFieldInfo.foreach { f =>
      f.y.foreach { xs =>
        fieldCount += (xs._2 - xs._1 + 1)
      }
    }
    fieldCount
  }

  def cleanSnakeTurnPoint(sid: String):Unit = {
    if (snakeTurnPoints.contains(sid)) {
      snakeTurnPoints -= sid
    }
  }

  def cleanDiedSnakeInfo(dieSnakes: List[String]): Unit = {
    snakeTurnPoints --= dieSnakes
    grid.foreach { g =>
      g._2 match {
        case Body(bid, fid) if fid.nonEmpty && dieSnakes.contains(bid) && dieSnakes.contains(fid.get) => grid -= g._1
        case Body(bid, fid) if fid.nonEmpty && dieSnakes.contains(bid) => grid += g._1 -> Field(fid.get)
        case Body(bid, _) if dieSnakes.contains(bid) => grid -= g._1
        case Body(bid, fid) if dieSnakes.contains(fid.getOrElse("")) => grid += g._1 -> Body(bid, None)
        case Field(fid) if dieSnakes.contains(fid) => grid -= g._1
        case _ =>
      }
    }
    snakes --= dieSnakes
  }

}

