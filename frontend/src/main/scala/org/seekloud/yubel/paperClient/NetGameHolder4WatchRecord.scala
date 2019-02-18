package org.seekloud.yubel.paperClient

import org.seekloud.yubel.common.Constant
import org.seekloud.yubel.paperClient.Protocol._
import org.scalajs.dom
import org.scalajs.dom.html.{Canvas, Document => _}
import org.scalajs.dom.raw._
import org.seekloud.yubel.paperClient.WebSocketProtocol._
import org.seekloud.yubel.util.Component

import scala.xml.Elem

/**
  * User: Taoz
  * Date: 9/1/2016
  * Time: 12:45 PM
  */
//@JSExportTopLevel("paperClient.NetGameHolder")
class NetGameHolder4WatchRecord(webSocketPara: WatchRecordPara) extends Component {

  var currentRank = List.empty[Score]
  var historyRank = List.empty[Score]
  private var myId = ""
  private var myMode = -1

  var grid = new GridOnClient(Point(BorderSize.w, BorderSize.h))

  var firstCome = true
  var justSynced = false
  var scoreFlag = true
  var isWin = false
  var winnerName = "unknown"
  var loading = true
  private var killInfo = ("", "", "")
  var lastTime = 0
  var winData: FrontProtocol.WinData4Draw = grid.getWinData4Draw
  var fieldNum = 1
  var snakeNum = 1
  var syncGridData4Replay: scala.Option[Protocol.Data4TotalSync] = None
  var snapshotMap = Map.empty[Int, Snapshot]
  var encloseMap = Map.empty[Int, List[FieldByColumn]]
  var spaceEvent = Map.empty[Int, NewSnakeInfo]
  var rankEvent = Map.empty[Int, RankEvent]
  var oldWindowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
  var replayFinish = false
  var gameLoopInterval = -1
  //  var pingInterval = -1
  var requestAnimationInterval = -1

  private var myScore = BaseScore(0, 0, 0)
  private var maxArea: Short = 0
  private var winningData = WinData(0, Some(0), "")


  //  private[this] val nameField = dom.document.getElementById("name").asInstanceOf[HTMLInputElement]
  //  private[this] val joinButton = dom.document.getElementById("join").asInstanceOf[HTMLButtonElement]
  private[this] val canvas = dom.document.getElementById("GameView").asInstanceOf[Canvas]
  private[this] val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private[this] val audio1 = dom.document.getElementById("audio").asInstanceOf[HTMLAudioElement]
  private[this] val audioFinish = dom.document.getElementById("audioFinish").asInstanceOf[HTMLAudioElement]
  private[this] val audioKill = dom.document.getElementById("audioKill").asInstanceOf[HTMLAudioElement]
  private[this] val audioKilled = dom.document.getElementById("audioKilled").asInstanceOf[HTMLAudioElement]
  private var nextFrame = 0
  private var isContinue = true
  private var logicFrameTime = System.currentTimeMillis()
  private[this] val drawGame: DrawGame = new DrawGame(ctx, canvas)
  private[this] val webSocketClient: WebSocketClient = new WebSocketClient(connectOpenSuccess, connectError, messageHandler, connectClose)


  def init(): Unit = {
    webSocketClient.setUp("watchRecord", webSocketPara)
  }


  def startGame(frameRate: Int): Unit = {
    println(s"start game======frameRate:$frameRate")
    drawGame.drawGameOn()
//    val frameRate = webSocketPara match {
//      case WebSocketProtocol.PlayGamePara(_, _, mode) =>
//        if(mode == 2) frameRate2 else frameRate1
//      case _ =>
//        frameRate1
//    }
    gameLoopInterval = dom.window.setInterval(() => gameLoop(), frameRate)
    //    pingInterval = dom.window.setInterval(() => {
    //      webSocketClient.sendMessage(SendPingPacket(myId, System.currentTimeMillis()).asInstanceOf[UserAction])
    //    }, 100)
    requestAnimationInterval = dom.window.requestAnimationFrame(gameRender(frameRate))
  }

  private var tempRender = System.currentTimeMillis()

  def gameRender(frameRate: Int): Double => Unit = { _ =>
    val curTime = System.currentTimeMillis()
    val offsetTime = curTime - logicFrameTime
    //    println(s"drawRender time:${curTime - tempRender}")
    tempRender = curTime
    draw(offsetTime, frameRate)

    if (isContinue)
      nextFrame = dom.window.requestAnimationFrame(gameRender(frameRate))
  }


  def gameLoop(): Unit = {
    logicFrameTime = System.currentTimeMillis()
    if ((oldWindowBoundary.x != dom.window.innerWidth.toFloat) || (oldWindowBoundary.y != dom.window.innerHeight.toFloat)) {
      drawGame.resetScreen()
      oldWindowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
      //      if(!isContinue) {
      //        if(isWin) {
      //          drawGame.drawGameWin(myId, winnerName, winData)
      //        } else {
      //          drawGame.drawGameDie(grid.getKiller(myId).map(_._2), myScore, maxArea)
      //        }
      //      }
    }

    if (webSocketClient.getWsState) {
      if (syncGridData4Replay.nonEmpty) {
        grid.initSyncGridData(syncGridData4Replay.get)
        syncGridData4Replay = None
        justSynced = false
      } else if (snapshotMap.contains(grid.frameCount)) {
        val data = snapshotMap(grid.frameCount)
        grid.initSyncGridData(Protocol.Data4TotalSync(grid.frameCount, data.snakes, data.bodyDetails, data.fieldDetails))
        //        println(s"state 重置 via Map")
        snapshotMap -= grid.frameCount
      }

      if (spaceEvent.contains(grid.frameCount)) {
        println(s"space event get !!!!")
        //        println(s"space event exists:${spaceEvent(grid.frameCount).id}, frame: ${grid.frameCount}")
//        replayMessageHandler(spaceEvent(grid.frameCount), grid.frameCount.toInt)
        val newSnakes = spaceEvent(grid.frameCount)
        if (newSnakes.snake.map(_.id).contains(myId) && !firstCome && !isContinue) spaceKey()
        newSnakes.snake.foreach { s => grid.cleanSnakeTurnPoint(s.id) } //清理死前拐点
        grid.snakes ++= newSnakes.snake.map(s => s.id -> s).toMap
        grid.addNewFieldInfo(newSnakes.filedDetails.map { f =>
          FieldByColumn(newSnakes.snake.find(_.yubelId == f.uid).get.id, f.scanField)
        })
        spaceEvent -= grid.frameCount
      }

      if (encloseMap.contains(grid.frameCount)) {
        encloseMap(grid.frameCount).map(_.uid).foreach { id =>
          grid.cleanSnakeTurnPoint(id)
        }
        //        grid.cleanTurnPoint4Reply(myId)
        grid.addNewFieldInfo(encloseMap(grid.frameCount))
        encloseMap -= grid.frameCount
        //        println(s"圈地 via Map")
      }

      if (!justSynced) { //前端更新
        grid.update("f")

      }
    }
  }


  def draw(offsetTime: Long, frameRate: Int): Unit = {
    if (webSocketClient.getWsState) {
      if (replayFinish) {
        drawGame.drawGameOff(firstCome, Some(true), false, false)
      } else if (loading) {
        drawGame.drawGameOff(firstCome, Some(false), true, false)
      } else {
        val data = grid.getGridData4Draw(myId, drawGame.fieldScale)
        if (isWin) {
          ctx.clearRect(0, 0, dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
          drawGame.drawGameWin(myId, winnerName, winData,winningData)
          audio1.play()
          dom.window.cancelAnimationFrame(nextFrame)
          isContinue = false
        } else {
          data.snakes.find(_.id == myId) match {
            case Some(snake) =>
              firstCome = false
              if (scoreFlag) {
                myScore = BaseScore(0, 0, 0)
                //                drawGame.cleanMyScore
                scoreFlag = false
              }
              //              data.killHistory.foreach {
              //                i => if (i.frameCount + 1 == data.frameCount && i.killerId == myId) audioKill.play()
              //              }
              if (killInfo._3 == myId) audioKill.play()
              var num = 0
              data.fieldDetails.find(_.uid == myId).get.scanField.foreach {
                row =>
                  row.y.foreach {
                    x => num += (x._2 - x._1)
                  }
              }
              if (fieldNum < num && snake.id == myId) {
                audioFinish.play()
              }
              fieldNum = num
              drawGameImage(myId, data, offsetTime, frameRate)
              if (killInfo._2 != "" && killInfo._3 != "" && snake.id != killInfo._1) {
                drawGame.drawBarrage(killInfo._2, killInfo._3)
                lastTime -= 1
                if (lastTime == 0) {
                  killInfo = ("", "", "")
                }
              }

            case None =>
              if (firstCome) drawGame.drawGameWait()
              else {
                if (isContinue) audioKilled.play()
                currentRank.filter(_.id == myId).foreach { score =>
                  myScore = myScore.copy(kill = score.k, area = score.area)
                }
//                drawGame.drawGameDie(grid.getKiller(myId).map(_._2), myScore, maxArea, true)
                drawGame.drawGameDie4Replay()
                killInfo = ("", "", "")
                dom.window.cancelAnimationFrame(nextFrame)
                isContinue = false
              }
          }
        }
      }
    } else {
      drawGame.drawGameOff(firstCome, None, false, false)
    }
  }

  def drawGameImage(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long, frameRate: Int): Unit = {
    drawGame.drawGrid(uid, data, offsetTime, grid, currentRank.headOption.map(_.id).getOrElse(myId), true, frameRate = frameRate)
    drawGame.drawSmallMap(data.snakes.filter(_.id == uid).map(_.header).head, data.snakes.filterNot(_.id == uid))
    //    drawGame.drawRank(myId, grid.getGridData.snakes, currentRank)
  }

  private def connectOpenSuccess(event0: Event, order: String) = {
    //    startGame()
    event0
  }

  private def connectError(e: Event) = {
    drawGame.drawGameOff(firstCome, None, false, false)
    e
  }

  private def connectClose(e: Event, s: Boolean) = {
    if(s)
      drawGame.drawGameOff(firstCome, None, false, false)
    else
      drawGame.drawServerShutDown()
    e
  }

  private def messageHandler(data: GameMessage): Unit = {
    data match {

      case Protocol.InitReplayError(msg) =>
        drawGame.drawGameOff(true, None, false, true)

      case Protocol.Id(id) => myId = id
        println(s"receive ID = $id")

      case Protocol.Mode(mode) =>
        println(s"receive mode = $mode")
        myMode = mode

      case Protocol.StartLoading(frame) =>
        println(s"start loading  =========")
        replayFinish = false
        dom.window.clearInterval(gameLoopInterval)
        //        dom.window.clearInterval(pingInterval)
        dom.window.clearInterval(requestAnimationInterval)
        loading = true
        drawGame.drawGameOff(firstCome, Some(false), loading, false)
        grid.frameCount = frame
        grid.initSyncGridData(Protocol.Data4TotalSync(grid.frameCount, List(), List(), List()))
        snapshotMap = Map.empty[Int, Snapshot]
        encloseMap = Map.empty[Int, List[FieldByColumn]]


      case Protocol.StartReplay(firstSnapshotFrame, firstReplayFrame) =>
        //        println(s"firstSnapshotFrame::$firstSnapshotFrame")
        //        println(s"firstReplayFrame::$firstReplayFrame")
        for (i <- firstSnapshotFrame until firstReplayFrame - 1) {
          if (webSocketClient.getWsState) {
            if (snapshotMap.contains(grid.frameCount)) {
              val data = snapshotMap(grid.frameCount)
              grid.initSyncGridData(Protocol.Data4TotalSync(grid.frameCount, data.snakes, data.bodyDetails, data.fieldDetails))
              if (data.snakes.exists(_.id == myId)) firstCome = false
              //        println(s"state 重置 via Map")
              snapshotMap = snapshotMap.filter(_._1 > grid.frameCount)
            }
            if (spaceEvent.contains(grid.frameCount)) {
              replayMessageHandler(spaceEvent(grid.frameCount), grid.frameCount.toInt)
              spaceEvent -= grid.frameCount
            }
            if (rankEvent.contains(grid.frameCount)) {
              replayMessageHandler(rankEvent(grid.frameCount), grid.frameCount.toInt)
              rankEvent -= grid.frameCount
            }

            if (encloseMap.contains(grid.frameCount)) {
              encloseMap(grid.frameCount).map(_.uid).foreach { id =>
                grid.cleanSnakeTurnPoint(id)
              }
              grid.addNewFieldInfo(encloseMap(grid.frameCount))
              encloseMap -= grid.frameCount
              //        println(s"圈地 via Map")
            }
            grid.update("f")
          }

        }
        //        val snakes = grid.getGridData.snakes.map(_.id)
        //        println(s"snakes:::::$snakes")
        if (!isContinue) firstCome = false
        loading = false
        val frameRate = myMode match {
          case 2 => frameRate2
          case _ => frameRate1
        }
        startGame(frameRate)
      //        grid.frameCount = firstReplayframe.toLong

      case x@Protocol.ReplayFinish(_) =>
        println("get message replay finish")
        replayFinish = true

      case Protocol.ReplayFrameData(frameIndex, eventsData, stateData) =>
        //        println(s"replayFrameData,grid.frameCount:${grid.frameCount},frameIndex:$frameIndex")
//        println(s"grid.frameCount:${grid.frameCount}")
//        println(s"frameIndex     :$frameIndex")
        if (frameIndex == 0) grid.frameCount = 0
        if (stateData.nonEmpty) {
          stateData.get match {
            case msg: Snapshot =>
              //              println(s"snapshot get:$msg")
              replayMessageHandler(msg, frameIndex + 1)
            case Protocol.DecodeError() =>
              println("state decode error")
            case _ =>
          }
        }


        eventsData match {
          case EventData(events) =>
            //            println(s"eventsData:$eventsData")
            events.foreach { event =>
              (event, loading) match {
                case (EncloseEvent(_), true) => replayMessageHandler(event, frameIndex)
                case (DirectionEvent(_, _), true) => replayMessageHandler(event, frameIndex)
                case (e@NewSnakeInfo(_, _), _) => spaceEvent += (frameIndex -> e)
//                case (e@Protocol.NewData(_, newSnakes, newField), true) => replayMessageHandler(e, frameIndex)
                case (e@RankEvent(_), true) => rankEvent += (frameIndex -> e)
                case (_, false) => replayMessageHandler(event, frameIndex)
                case _ =>
              }
            }

          case Protocol.DecodeError() =>
          //            println("events decode error")
          case _ =>
        }


      case x@_ =>
        println(s"receive unknown msg:$x")
    }
  }


  private def replayMessageHandler(data: GameEvent, frameIndex: Int): Unit = {
    data match {
      case Protocol.JoinEvent(id, snakeInfo) =>
      //        println(s"receive data: $data")
      //        println(s"grid.frameCount:${grid.frameCount}")
      //        if (grid.frameCount < frameIndex) {
      //          if(joinOrLeftMap.get(frameIndex).nonEmpty) {
      //            joinOrLeftMap += ((frameIndex, data :: joinOrLeftMap(frameIndex)))
      //          } else {
      //            joinOrLeftMap += ((frameIndex, List(data)))
      //          }
      //        } else {
      //          grid.snakes += (id -> snakeInfo.get)
      //        }



      case Protocol.LeftEvent(id, name) =>
      //        if (grid.frameCount < frameIndex) {
      //          if(joinOrLeftMap.get(frameIndex).nonEmpty) {
      //            joinOrLeftMap += ((frameIndex, data :: joinOrLeftMap(frameIndex)))
      //          } else {
      //            joinOrLeftMap += ((frameIndex, List(data)))
      //          }
      //        } else {
      //          grid.snakes -= id
      //        }
      case Protocol.SomeOneWin(winner) =>
        val finalData = grid.getWinData4Draw
        println(s"recv someONeWin==============, winner:$winner")
        isWin = true
        winnerName = winner
        winData = finalData
        grid.cleanData()

//      case Protocol.UserDead(frame, id, name, killerName) =>
//        deadUser += frame -> (deadUser.getOrElse(frame, Nil) ::: List(id))
//        if (killerName.nonEmpty) {
//          killInfo = Some(id, name, killerName.get)
//          barrageDuration = 100
//        }


      case DirectionEvent(id, keyCode) =>
        grid.addActionWithFrame(id, keyCode, frameIndex)

      case msg@SpaceEvent(id) =>
        //        println(s"get space event:$id, frame: $frameIndex")
//        if (grid.frameCount < frameIndex) {
//          spaceEvent += (frameIndex -> msg)
//        } else {
//          if (id == myId) {
//            audio1.pause()
//            audio1.currentTime = 0
//            audioKilled.pause()
//            audioKilled.currentTime = 0
//            scoreFlag = true
//            firstCome = true
//            myScore = BaseScore(0, 0, 0)
//            if (isWin) {
//              isWin = false
//              winnerName = "unknown"
//            }
//            isContinue = true
//            val frameRate = myMode match {
//              case 2 => frameRate2
//              case _ => frameRate1
//            }
//            nextFrame = dom.window.requestAnimationFrame(gameRender(frameRate))
//
//          }
//        }

      case x@Protocol.WinData(winnerScore, yourScore, winner) =>
        if (winner == myId) maxArea = Constant.shortMax(maxArea, winnerScore)
        println(s"receive winningData msg:$x")
        winningData = x

//      case Protocol.SomeOneKilled(killedId, killedName, killerName) =>
//        killInfo = (killedId, killedName, killerName)
//        lastTime = 100

      case EncloseEvent(enclosure) =>
        //        println(s"got enclose event")
        //        println(s"当前帧号：${grid.frameCount}")
        //        println(s"传输帧号：$frameIndex")
        if (grid.frameCount < frameIndex) {
          encloseMap += (frameIndex -> enclosure)
        } else if (grid.frameCount == frameIndex) {
          //          println(s"圈地")
          //          println(s"enclosure:$enclosure")
          enclosure.map(_.uid).foreach { id =>
            grid.cleanSnakeTurnPoint(id)
          }
          //          grid.cleanTurnPoint4Reply(myId)
          grid.addNewFieldInfo(enclosure)
        }

      case RankEvent(current) =>
        currentRank = current
        maxArea = Constant.shortMax(currentRank.find(_.id == myId).map(_.area).getOrElse(0), maxArea)
        if (grid.getGridData.snakes.exists(_.id == myId) && !isWin) drawGame.drawRank4Replay(myId, grid.getGridData.snakes, currentRank)


      case msg@Snapshot(snakes, bodyDetails, fieldDetails) =>
        //        println(s"snapshot, frame:$frameIndex, snakes:${snakes.map(_.id)}")
        snapshotMap += frameIndex -> msg
        if (grid.frameCount >= frameIndex.toLong) { //重置
          syncGridData4Replay = Some(Protocol.Data4TotalSync(frameIndex + 1, snakes, bodyDetails, fieldDetails))
          justSynced = true
        }

      case _ =>
    }
  }

  import org.seekloud.byteobject.ByteObject._
  import org.seekloud.byteobject.MiddleBufferInJs

  import scala.scalajs.js.typedarray.ArrayBuffer

  private def replayEventDecode(a: ArrayBuffer): GameEvent = {
    val middleDataInJs = new MiddleBufferInJs(a)
    if (a.byteLength > 0) {
      bytesDecode[List[GameEvent]](middleDataInJs) match {
        case Right(r) =>
          Protocol.EventData(r)
        case Left(e) =>
          Protocol.DecodeError()
      }
    } else {
      Protocol.DecodeError()
    }
  }

  def spaceKey(): Unit = {
//    audio1.pause()
//    audio1.currentTime = 0
//    audioKilled.pause()
//    audioKilled.currentTime = 0
    scoreFlag = true
    firstCome = true
    myScore = BaseScore(0, 0, 0)
    if (isWin) {
      isWin = false
      winnerName = "unknown"
    }
    isContinue = true
    val frameRate = myMode match {
      case 2 => frameRate2
      case _ => frameRate1
    }
    nextFrame = dom.window.requestAnimationFrame(gameRender(frameRate))
  }

  private def replayStateDecode(a: ArrayBuffer): GameEvent = {
    val middleDataInJs = new MiddleBufferInJs(a)
    bytesDecode[Snapshot](middleDataInJs) match {
      case Right(r) =>
        r
      case Left(e) =>
        println(e.message)
        Protocol.DecodeError()
    }
  }

  override def render: Elem = {
    init()
    <div></div>
  }
}
