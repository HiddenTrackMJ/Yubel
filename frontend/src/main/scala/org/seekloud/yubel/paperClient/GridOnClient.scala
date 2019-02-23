package org.seekloud.yubel.paperClient

import java.awt.event.KeyEvent

import org.seekloud.yubel.paperClient.Protocol.{NewFieldInfo, Point4Trans, getBoardWidth}

import scala.collection.mutable

/**
  * User: Taoz
  * Date: 9/3/2016
  * Time: 10:13 PM
  */
class GridOnClient(override val boundary: Point) extends Grid {

  override def debug(msg: String): Unit = println(msg)

  override def info(msg: String): Unit = println(msg)

  var yubelMap = Map.empty[Byte, String]
  var fieldDrawMap = mutable.Map.empty[Int, mutable.Map[String, mutable.Map[Short, List[Short]]]] //(frameCount, List[Field4Draw])


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


          val newFrame = frameCount + 1
          historyFieldInfo.get(newFrame).foreach { data =>
            addNewFieldInfo(data)
          }

          historyDieSnake.get(newFrame).foreach { dieSnakes =>
            cleanDiedBoardInfo(dieSnakes)
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


  def updateBoardOnClient(): Unit = {
    updateBoards()
    updateBalls()
    yubelMap = boardMap.map(s => s._2.yubelId -> s._1)
    val limitFrameCount = frameCount - (maxDelayed + 1)
    actionMap = actionMap.filter(_._1 > limitFrameCount)
    boardActionMap = boardActionMap.filter(_._1 > limitFrameCount)
    frameCount += 1
  }


  def searchMyField(uid: String) = {
    val map = mutable.Map.empty[String, List[Point]]
    grid.foreach { g =>
      g._2 match {
        case Field(fid) if fid == uid =>
          map += fid -> (g._1 :: map.getOrElse(fid, List.empty[Point]))
        case _ =>
      }
    }
    map
  }
}
