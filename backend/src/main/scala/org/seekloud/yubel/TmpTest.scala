package org.seekloud.yubel

import org.seekloud.yubel.paperClient._

/**
  * User: Taoz
  * Date: 6/28/2018
  * Time: 7:26 PM
  */
object TmpTest extends Grid{

  val boundary: Point = Point(BorderSize.w, BorderSize.h)

  override def debug(msg: String): Unit = println(msg)

  override def info(msg: String): Unit = println(msg)

  val baseDirection = Map("left" -> Point(-1, 0), "right" -> Point(1, 0), "up" -> Point(0, -1), "down" -> Point(0, 1))

  def findShortestPath(start: Point, end: Point, fieldBoundary: List[Point], turnPoint: List[(Point, Point)]) = {
    val startDirection = findClockwiseDirection(start, fieldBoundary)
    if(startDirection != Point(-1, -1)){
      val route1 = getShortest(start + startDirection, end, fieldBoundary, List(start + startDirection, start), startDirection, turnPoint)
      if(route1.nonEmpty){
        val route2 = (fieldBoundary.toSet &~ route1.toSet).toList ::: List(start, end)
        if(route1.lengthCompare(route2.length) > 0) (route2, route1) else (route1, route2)
      } else {
        (Nil, Nil)
      }
    } else{
      (Nil, Nil)
    }
  }

  def getShortest(start: Point, end: Point, fieldBoundary: List[Point], targetPath: List[Point], lastDirection: Point, turnPoint: List[(Point, Point)]): List[Point] = {
    var res = targetPath
    val nextDirection = if (turnPoint.exists(_._1 == start)) turnPoint.filter(_._1 == start).head._2 else lastDirection
    if (start - end != Point(0, 0)) {
      if (fieldBoundary.contains(start + nextDirection) && !targetPath.contains(start + nextDirection)) {
        res = getShortest(start + nextDirection, end, fieldBoundary, start + nextDirection :: targetPath, nextDirection, turnPoint)
      } else {
        val tryAgain = clockwise(start, lastDirection, fieldBoundary)
        if (fieldBoundary.contains(start + tryAgain)  && !targetPath.contains(start + nextDirection)) {
          res = getShortest(start + tryAgain, end, fieldBoundary, start + tryAgain :: targetPath, nextDirection, turnPoint)
        } else
          return Nil
      }
    }
    res
  }

  def clockwise(nowPoint: Point, lastDirection: Point, boundarys: List[Point]) = {
    var direction = Point(-1, -1)
    val directions = baseDirection.map(_._2).filterNot(_ == lastDirection).filter(i => boundarys.contains(nowPoint + i))
    if (directions.size == 1) {
      direction = directions.head
    } else {
      directions.foreach { i =>
        if (boundarys.contains(nowPoint + i) && !boundarys.contains(nowPoint + i - lastDirection))
          direction = i
      }
    }
    direction
  }

  def findClockwiseDirection(startPoint:Point, boundarys: List[Point]) = {
    var target = Point(-1 ,-1)
    baseDirection.filter(d => boundarys.contains(startPoint + d._2)).foreach{ i=>
      i._1 match {
        case "up" =>
          if(!boundarys.contains(startPoint + i._2 + baseDirection("left")))
            target = i._2

        case "down" =>
          if(!boundarys.contains(startPoint + i._2 + baseDirection("right")))
            target = i._2

        case "left" =>
          if(!boundarys.contains(startPoint + i._2 + baseDirection("down")))
            target = i._2

        case "right" =>
          if(!boundarys.contains(startPoint + i._2 + baseDirection("up")))
            target = i._2
      }
    }
    target
  }


  def findInsidePoint(point1: Point, point2: Point, boundary: List[Point]): Point = {
    if (boundary.count(p => p.x == point1.x && p.y > point1.y) % 2 == 1 &&
      boundary.count(p => p.x == point1.x && p.y < point1.y) % 2 == 1 &&
      boundary.count(p => p.y == point1.y && p.x > point1.x) % 2 == 1 &&
      boundary.count(p => p.y == point1.y && p.x < point1.x) % 2 == 1) { //射线上相交个数均为奇数的点为内部点
      point1
    } else {
      point2
    }
  }

  def getVertex() ={
    var fields: List[Fd] = Nil
    grid.foreach {
      case (p, Field(id)) => fields ::= Fd(id, p.x.toInt, p.y.toInt)
      case _ => //doNothing
    }

    val a =  fields.groupBy(_.id).map{ case (uid,fieldPoints) =>
      fieldPoints.filter{p => {
        var counter = 0
        val pointList = List(Point(-1,1),Point(-1,-1),Point(1,1),Point(1,-1),
          Point(0,1),Point(-1,0),Point(0,-1),Point(1,0))
        pointList.foreach{ i =>
          if (getPointBelong(uid,Point(p.x,p.y) + i)) counter += 1
        }
        counter match {
          case 4 => true
          case 3 => true
          case 7 => true
          case _ => false
        }
      }
      }.filter{p =>
        var counter = 0
        val pointList = List(Point(0,1),Point(-1,0),Point(0,-1),Point(1,0))
        pointList.foreach{ i =>
          if (getPointBelong(uid,Point(p.x,p.y) + i)) counter += 1
        }
        counter match {
          case 3 => false
          case _ => true
        }
      }
    }
    println("顶点：" + a)
  }


  def main(args: Array[String]): Unit = {
//    getGridData
    (1 to 10).toList.reverse.foreach(i =>
    println(i))
  }
}
