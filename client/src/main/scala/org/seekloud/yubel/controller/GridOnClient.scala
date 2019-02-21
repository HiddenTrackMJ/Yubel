package org.seekloud.yubel.controller

import java.awt.event.KeyEvent

import org.seekloud.yubel.paperClient.Protocol.{NewFieldInfo, Point4Trans}
import org.seekloud.yubel.paperClient._

/**
  * User: Taoz
  * Date: 9/3/2016
  * Time: 10:13 PM
  */
class GridOnClient(override val boundary: Point) extends Grid {

  override def debug(msg: String): Unit = println(msg)

  override def info(msg: String): Unit = println(msg)

  var myActionHistory : Map[Int, (Int, Int)] = Map[Int, (Int, Int)]() //(actionId, (keyCode, frameCount))

  //击杀弹幕
  var killInfo: scala.Option[(String, String, String, String)] = None
  var barrageDuration = 0

  var yubelMap = Map.empty[Byte, String]


  def initSyncGridData(data: Protocol.Data4TotalSync): Unit = {
    println("back frame:" + data.frameCount)
    val gridField = grid.filter(_._2 match { case Field(_) => true case _ => false })
    var gridMap = grid.filter(_._2 match { case Body(_, _) => false case _ => true })

    data.bodyDetails.foreach { bodies =>
      val uid = bodies.uid
      if (bodies.turn.turnPoint.nonEmpty) {
        var first = bodies.turn.turnPoint.head
        var remainder = bodies.turn.turnPoint.tail
        while (remainder.nonEmpty) {
          if (first.x == remainder.head.x) { //同x
            (Math.min(first.y, remainder.head.y) to Math.max(first.y, remainder.head.y)).foreach(y =>
              gridMap += Point(first.x, y) -> Body(uid, None)
            )
          } else { // 同y
            (Math.min(first.x, remainder.head.x) to Math.max(first.x, remainder.head.x)).foreach(x =>
              gridMap += Point(x, first.y) -> Body(uid, None)
            )
          }
          first = remainder.head
          remainder = remainder.tail
        }
      }
      bodies.turn.pointOnField.foreach { p => gridMap += Point(p._1.x, p._1.y) -> Body(uid, Some(p._2)) }
      snakeTurnPoints += ((uid, bodies.turn.turnPoint))
    }

    gridMap ++= gridField

    if (data.fieldDetails.nonEmpty) {
      gridMap = gridMap.filter(_._2 match { case Field(_) => false case _ => true })
      data.fieldDetails.foreach { baseInfo =>
        baseInfo.scanField.foreach { fids =>
          fids.y.foreach { ly =>
            (ly._1 to ly._2 by 1).foreach { y =>
              fids.x.foreach { lx => (lx._1 to lx._2 by 1).foreach(x => gridMap += Point(x, y) -> Field(baseInfo.uid)) }
            }
          }
        }
      }
    }

    frameCount = data.frameCount
    grid = gridMap
    actionMap = actionMap.filterKeys(_ >= (data.frameCount - maxDelayed))
    snakes = data.snakes.map(s => s.id -> s).toMap
    yubelMap = data.snakes.map(s => s.yubelId -> s.id).toMap
  }

  def addNewFieldInfo(data: List[Protocol.FieldByColumn]): Unit = {
    data.foreach { baseInfo =>
      baseInfo.scanField.foreach { fids =>
        fids.y.foreach { ly =>
          (ly._1 to ly._2 by 1).foreach { y =>
            fids.x.foreach { lx =>
              (lx._1 to lx._2 by 1).foreach { x =>
                grid.get(Point(x, y)) match {
                  case Some(Body(bid, _)) => grid += Point(x, y) -> Body(bid, Some(baseInfo.uid))
                  case _ => grid += Point(x, y) -> Field(baseInfo.uid)
                }
              }
            }
          }
        }
      }
    }
  }

  def recallGrid(startFrame: Int, endFrame: Int): Unit = {
    hsMap.get(startFrame) match {
      case Some(state) =>
        println(s"recallGrid-start$startFrame-end-$endFrame")
        snakes = state._1
        grid = state._2
        snakeTurnPoints = state._3
        (startFrame until endFrame).foreach { frame =>
          frameCount = frame

          updateSnakesOnClient()
          updateSpots()

          val newFrame = frameCount + 1
          historyFieldInfo.get(newFrame).foreach { data =>
            addNewFieldInfo(data)
          }

          historyDieSnake.get(newFrame).foreach { dieSnakes =>
            cleanDiedSnakeInfo(dieSnakes)
          }

          historyNewSnake.get(newFrame).foreach { newSnakes =>
            newSnakes._1.foreach { s => cleanSnakeTurnPoint(s.id) } //清理死前拐点
            snakes ++= newSnakes._1.map(s => s.id -> s).toMap
            addNewFieldInfo(newSnakes._2)
          }
        }
        frameCount += 1

      case None =>
        println(s"???can't find-$startFrame-end is $endFrame!!!!tartget-${hsMap.keySet}")
    }
  }

  def setGridInGivenFrame(frame: Int): Unit = {
    frameCount = frame
    val state = hsMap(frame)
    snakes = state._1
    grid = state._2
    snakeTurnPoints = state._3
  }

  def findRecallFrame(receiveFame: Int, oldRecallFrame: Option[Int]): Option[Int] = {
    if (hsMap.get(receiveFame).nonEmpty) { //回溯
      oldRecallFrame match {
        case Some(oldFrame) => Some(Math.min(receiveFame, oldFrame))
        case None => Some(receiveFame)
      }
    } else {
      Some(-1)
    }
  }

  def updateOnClient(): Unit = {
    updateSnakesOnClient()
    super.updateSpots()
    val limitFrameCount = frameCount - (maxDelayed + 1)
    actionMap = actionMap.filter(_._1 > limitFrameCount)
    historyFieldInfo = historyFieldInfo.filter(_._1 > limitFrameCount)
    hsMap = hsMap.filter(_._1 > limitFrameCount)
    historyNewSnake = historyNewSnake.filter(_._1 > limitFrameCount)
    historyDieSnake = historyDieSnake.filter(_._1 > limitFrameCount)
    frameCount += 1
  }

  def updateSnakesOnClient(): Unit = {
    def updateASnake(snake: SkDt, actMap: Map[String, Int]): UpdateSnakeInfo = {
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
            //            debug(s"snake[${snake.id}] hit wall.")
            grid.get(snake.header) match { //当上一点是领地时 记录出行的起点
              case Some(Field(fid)) if fid == snake.id =>
                snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header), x.fid)

              case Some(Body(bid, _)) if bid == snake.id && x.fid.getOrElse(-1L) == snake.id =>
                returnBackField(snake.id)
                UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), Some(snake.id))

              case _ =>
                if (snake.direction != newDirection)
                  snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(snake.header.x.toShort, snake.header.y.toShort))))
                UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), x.fid)
            }

          case Some(Field(id)) =>
            if (id == snake.id) {
              grid.get(snake.header) match {
                case Some(Body(bid, _)) if bid == snake.id => //回到了自己的领域
                  returnBackField(snake.id)
                  UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), Some(snake.id))

                case _ =>
                  UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), Some(id))
              }
            } else { //进入到别人的领域
              grid.get(snake.header) match { //当上一点是领地时 记录出行的起点
                case Some(Field(fid)) if fid == snake.id =>
                  snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                  UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header), Some(id))
                case _ =>
                  if (snake.direction != newDirection)
                    snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(snake.header.x.toShort, snake.header.y.toShort))))
                  UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection), Some(id))
              }
            }

          case Some(Border) =>
            UpdateSnakeInfo(snake.copy(header = snake.header, direction = Point(0,0)), None)

          case _ =>
            grid.get(snake.header) match { //当上一点是领地时 记录出行的起点
              case Some(Field(fid)) if fid == snake.id =>
                snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(newHeader.x.toShort, newHeader.y.toShort))))
                UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection, startPoint = snake.header))

              case _ =>
                if (snake.direction != newDirection)
                  snakeTurnPoints += ((snake.id, snakeTurnPoints.getOrElse(snake.id, Nil) ::: List(Point4Trans(snake.header.x.toShort, snake.header.y.toShort))))
                UpdateSnakeInfo(snake.copy(header = newHeader, direction = newDirection))
            }
        }
      }
      else UpdateSnakeInfo(snake, Some(snake.id))

    }

    hsMap += frameCount -> (snakes, grid, snakeTurnPoints)

    val acts = actionMap.getOrElse(frameCount, Map.empty[String, Int])

    val updatedSnakes = snakes.values.map(updateASnake(_, acts)).toList

    updatedSnakes.foreach { s =>
      if (s.bodyInField.nonEmpty && s.bodyInField.get == s.data.id) grid += s.data.header -> Field(s.data.id)
      else grid += s.data.header -> Body(s.data.id, s.bodyInField)
    }

    snakes = updatedSnakes.map(s => (s.data.id, s.data)).toMap
  }

  def getGridData4Draw: FrontProtocol.Data4Draw = {
    var fields = Map.empty[String, Map[Short, List[Short]]]
    val bodyDetails = snakes.values.map { s => FrontProtocol.BodyInfo4Draw(s.id, getMyTurnPoint(s.id, s.header)) }.toList

    grid.foreach {
      case (p, Field(id)) =>
        val map = fields.getOrElse(id, Map.empty)
        fields += (id -> (map + (p.x.toShort -> (p.y.toShort :: map.getOrElse(p.x.toShort, Nil)))))

      case _ => //doNothing
    }

    val fieldDetails = fields.map { f =>
      FrontProtocol.Field4Draw(f._1, f._2.map { p =>
        FrontProtocol.Scan4Draw(p._1, Tool.findContinuous(p._2.sorted))
      }.toList)
    }.toList

    FrontProtocol.Data4Draw(
      frameCount,
      snakes.values.toList,
      bodyDetails,
      fieldDetails
    )
  }

  def getWinData4Draw: FrontProtocol.WinData4Draw = {
    var fields = Map.empty[String, Map[Short, List[Short]]]

   

    grid.foreach {
      case (p, Field(id)) =>
        val map = fields.getOrElse(id, Map.empty)
        fields += (id -> (map + (p.x.toShort -> (p.y.toShort :: map.getOrElse(p.x.toShort, Nil)))))

      case (p, Body(_, Some(id))) =>
        val map = fields.getOrElse(id, Map.empty)
        fields += (id -> (map + (p.x.toShort -> (p.y.toShort :: map.getOrElse(p.x.toShort, Nil)))))

      case _ => //doNothing
    }

    

    val fieldDetails = fields.map { f =>
      FrontProtocol.Field4Draw(f._1, f._2.map { p =>
        FrontProtocol.Scan4Draw(p._1, Tool.findContinuous(p._2.sorted))
      }.toList)
    }.toList



    FrontProtocol.WinData4Draw(
      frameCount,
      snakes.values.toList,
      fieldDetails
    )
  }

  def getMyTurnPoint(sid:String, header: Point): List[Protocol.Point4Trans] = {
    val turnPoint = snakeTurnPoints.getOrElse(sid, Nil)
    if (turnPoint.nonEmpty) {
      turnPoint ::: List(Point4Trans(header.x.toShort, header.y.toShort))
    } else Nil
  }

}
