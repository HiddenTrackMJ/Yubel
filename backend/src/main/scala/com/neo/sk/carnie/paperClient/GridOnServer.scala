package com.neo.sk.carnie.paperClient

import org.slf4j.LoggerFactory
import com.neo.sk.carnie.paperClient.Protocol._
import org.seekloud.byteobject.MiddleBufferInJvm

/**
  * User: Taoz
  * Date: 9/3/2016
  * Time: 9:55 PM
  */
class GridOnServer(override val boundary: Point) extends Grid {


  private[this] val log = LoggerFactory.getLogger(this.getClass)

  override def debug(msg: String): Unit = log.debug(msg)

  override def info(msg: String): Unit = log.info(msg)

  private[this] var waitingJoin = Map.empty[String, (String, String)]

  private val maxRecordNum = 100

  private val fileMaxRecordNum = 100000000

  var fileRecordNum = 0

  val currentTime = System.currentTimeMillis()

  val fileName = s"carnie_${currentTime}"

//  val initState = Snapshot(grid, snakes, Nil)

  var fileIndex = 0

//  var recorder: FrameOutputStream = getRecorder(fileName, fileIndex, GameInformation(currentTime), Some(initState))

  val middleBuffer = new MiddleBufferInJvm(10 * 4096)

  var eventFrames = List.empty[Option[(List[GameEvent], Option[Snapshot])]]

  var enclosureEveryFrame = List.empty[(Long, List[Point])]

  var stateEveryFrame :Option[Snapshot] = None

  var currentRank = List.empty[Score]

  private[this] var historyRankMap = Map.empty[String, Score]
  var historyRankList = historyRankMap.values.toList.sortBy(_.k).reverse

  private[this] var historyRankThreshold = if (historyRankList.isEmpty) (-1, -1) else (historyRankList.map(_.area).min ,historyRankList.map(_.k).min)

  def addSnake(id: String, roomId:Int, name: String) = {
    val bodyColor = randomColor()
    waitingJoin += (id -> (name, bodyColor))
  }

  def waitingListState = waitingJoin.nonEmpty

  private[this] def genWaitingSnake() = {
    waitingJoin.filterNot(kv => snakes.contains(kv._1)).foreach { case (id, (name, bodyColor)) =>
      val startTime = System.currentTimeMillis()
      val indexSize = 5
      val basePoint = randomEmptyPoint(indexSize)
      (0 until indexSize).foreach { x =>
        (0 until indexSize).foreach { y =>
          grid += Point(basePoint.x + x, basePoint.y + y) -> Field(id)
        }
      }
      val startPoint = Point(basePoint.x + indexSize / 2, basePoint.y + indexSize / 2)
      snakes += id -> SkDt(id, name, bodyColor, startPoint, startPoint, startTime = startTime, endTime = startTime)
      killHistory -= id
    }
    waitingJoin = Map.empty[String, (String, String)]
  }

  implicit val scoreOrdering = new Ordering[Score] {
    override def compare(x: Score, y: Score): Int = {
      var r = y.area - x.area
      if (r == 0) {
        r = y.k - x.k
      }
      r
    }
  }

  private[this] def updateRanks() = {
    val areaMap = grid.filter { case (p, spot) =>
      spot match {
        case Field(id) if snakes.contains(id) => true
        case _ => false
      }
    }.map {
      case (p, f@Field(_)) => (p, f)
      case _ => (Point(-1, -1), Field((-1L).toString))
    }.filter(_._2.id != -1L).values.groupBy(_.id).map(p => (p._1, p._2.size))
    currentRank = snakes.values.map(s => Score(s.id, s.name, s.kill, areaMap.getOrElse(s.id, 0))).toList.sortBy(_.area).reverse
    var historyChange = false
    currentRank.foreach { cScore =>
      historyRankMap.get(cScore.id) match {
        case Some(oldScore) =>
          if(cScore.area > oldScore.area) {
            historyRankMap += (cScore.id -> cScore)
            historyChange = true
          }
          else if(cScore.area == oldScore.area && cScore.k > oldScore.k) {
            historyRankMap += (cScore.id -> cScore)
            historyChange = true
          }
        case None =>
          if(cScore.area > historyRankThreshold._1) {
            historyRankMap += (cScore.id -> cScore)
            historyChange = true
          }
          else if(cScore.area == historyRankThreshold._1 && cScore.k > historyRankThreshold._2) {
            historyRankMap += (cScore.id -> cScore)
            historyChange = true
          }
        case _ => //do nothing.
      }
    }

    if (historyChange) {
      historyRankList = historyRankMap.values.toList.sorted.take(historyRankLength)
      historyRankThreshold = (historyRankList.lastOption.map(_.area).getOrElse(-1), historyRankList.lastOption.map(_.k).getOrElse(-1))
      historyRankMap = historyRankList.map(s => s.id -> s).toMap
    }

  }

  def randomColor(): String = {
    var color = randomHex()
    val exceptColor = snakes.map(_._2.color).toList ::: List("#F5F5F5", "#000000", "#000080", "#696969") ::: waitingJoin.map(_._2._2).toList
    val similarityDegree = 2000
    while (exceptColor.map(c => colorSimilarity(c.split("#").last, color)).count(_<similarityDegree) > 0) {
      color = randomHex()
    }
    "#" + color
  }

  def randomHex() = {
    val h = random.nextInt(256).toHexString + random.nextInt(256).toHexString + random.nextInt(256).toHexString
    String.format("%6s", h).replaceAll("\\s", "0").toUpperCase()
  }

  def colorSimilarity(color1: String, color2: String) = {
    var target = 0.0
    var index = 0
    if(color1.length == 6 && color2.length == 6) {
      (0 until color1.length/2).foreach{ _ =>
        target = target +
          Math.pow(hexToDec(color1.substring(index, index + 2)) - hexToDec(color2.substring(index, index + 2)), 2)
        index = index + 2
      }
    }
    target.toInt
  }

  def hexToDec(hex: String): Int ={
    val hexString: String = "0123456789ABCDEF"
    var target = 0
    var base = Math.pow(16, hex.length - 1).toInt
    for(i <- 0 until hex.length){
      target = target + hexString.indexOf(hex(i)) * base
      base = base / 16
    }
    target
  }

  def updateInService(newSnake: Boolean): List[(String, List[Point])] = {
    val isFinish = super.update("b")
    if (newSnake) genWaitingSnake()
    updateRanks()
    isFinish
  }

  def getDirectionEvent(frameCount: Long): List[Protocol.DirectionEvent] = {
    actionMap.getOrElse(frameCount, Map.empty).toList.map(a => DirectionEvent(a._1, a._2))
  }
}
