package org.seekloud.carnie

import org.seekloud.carnie.paperClient.Protocol.Point4Trans

/**
  * User: Taoz
  * Date: 8/29/2016
  * Time: 9:48 PM
  */
package object paperClient {

  sealed trait Spot

  case class Body(id: String, fid: Option[String]) extends Spot

  case class Field(id: String) extends Spot

  case class Brick(bid: Int, bonus: Int, color: String) extends Spot

  case class Board(id: String,
                   color: String,
                   name: String,
                   center: Point,
                   direction: Point = Point(0, 0),
                   carnieId: Byte) extends Spot

  case class Ball( id: String,
                   color: String,
                   name: String,
                   center: Point,
                   direction: Point = Point(0, 0),
                   moveOrNot: Boolean ,
                   carnieId: Byte) extends Spot

  case class BrickSide(

                      )

  case object  Border extends Spot

  case object Blank extends Spot

  case class Score(id: String, n: String, k: Short, area: Short = 0)

  case class Bd(id: String, fid: Option[Long], x: Float, y: Float)

  case class Fd(id: String, x: Float, y: Float)

  case class Bord(x: Float, y: Float)

  case class KilledSkDt(
                         id: String,
                         nickname: String,
                         killing: Int,
                         score: Float,
                         startTime: Long,
                         endTime: Long
                       )

  case class BaseScore(kill: Short, area: Short, playTime: Short)


  case class Point(x: Float, y: Float) {
    def +(other: Point) = Point(x + other.x, y + other.y)

    def -(other: Point) = Point(x - other.x, y - other.y)

    def *(n: Int) = Point(x * n, y * n)

    def *(n: Float) = Point(x * n, y * n)

    def /(n: Int) = Point(x / n, y / n)

    def %(other: Point) = Point(x % other.x, y % other.y)

  }


  case class SkDt(
                   id: String,
                   name: String,
                   color: String,
                   startPoint: Point,
                   header: Point,
                   direction: Point = Point(0, 0),
                   kill: Short = 0,
                   img: Int = 0,
                   carnieId: Byte
                 )

  case class SkDt4Sync(
                        id: String,
                        startPoint: Point,
                        header: Point,
                        direction: Point
                      )

  case class UpdateSnakeInfo(
                              data: SkDt,
                              bodyInField: Option[String] = None
                            )


  object Boundary {
    val w = 200
    val h = 100
  }

  object BorderSize {
    val w = 60
    val h = 30
  }

  object Window {
    val w = 60
    val h = 30
  }

  object littleMap {
    val w = 12
    val h = 7
  }

}
