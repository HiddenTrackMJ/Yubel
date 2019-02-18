package org.seekloud.yubel.paperClient

/**
  * Created by dry on 2018/9/7.
  **/
object Tool {
  def findContinuous(points: List[Short]): List[(Short, Short)] = {
    var target = List.empty[(Short, Short)]
    if (points.max - points.min == (points.length - 1)) { //仅存在一段
      target = List((points.min, points.max))
    } else { //分段寻找
      var start = points.head
      (0 until points.length - 1).foreach { i =>
        if (points(i) + 1 == points(i + 1)) {
          if (i + 1 == points.length - 1) {
            target = target ::: List((start, points(i + 1)))
          }
        } else {
          target = target ::: List((start, points(i)))
          start = points(i + 1)
          if (i + 1 == points.length - 1) {
            target = target ::: List((start, start))
          }
        }
      }
    }
    target
  }

  def main(args: Array[String]): Unit = {
//    findContinuous(Array(1,2,3,4,5,6,7,8,9,10,11))
    println(123)
  }

}
