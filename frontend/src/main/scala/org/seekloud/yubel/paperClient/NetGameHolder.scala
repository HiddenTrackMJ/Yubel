package org.seekloud.yubel.paperClient

import java.util.concurrent.atomic.AtomicInteger

import org.seekloud.yubel.Routes
import org.seekloud.yubel.common.Constant
import org.scalajs.dom.html.Canvas
import org.seekloud.yubel.paperClient.Protocol._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html.{Document => _, _}
import org.scalajs.dom.raw._
import org.seekloud.yubel.paperClient.WebSocketProtocol._
import org.seekloud.yubel.ptcl.SuccessRsp
import org.seekloud.yubel.util.{Component, Http}
import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.xml.Elem

/**
  * User: Tao
  * Date: 9/1/2016
  * Time: 12:45 PM
  */

class NetGameHolder(order: String, webSocketPara: WebSocketPara, mode: Int, img: Int = 0, frameRate: Int = 75) extends Component {
  //0:正常模式，1:反转模式, 2:2倍加速模式

  var currentRank = List.empty[Sc]
  var historyRank = List.empty[Sc]
  private var myId = ""
  var myTrueId = ""

  var grid = new GridOnClient(Point(BorderSize.w, BorderSize.h))

  var isGetKiller = false
  var killerInfo: scala.Option[String] = None
  var firstCome = true
  var isSynced = false
  var isWin = false
  var isPlay = true
  var killInfo: scala.Option[(String, String, String, String)] = None
  var barrageDuration = 0

  var syncFrame: scala.Option[Protocol.SyncFrame] = None
  var syncGridData: scala.Option[Protocol.AllData] = None
  var isContinue = true
  var oldWindowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
  var drawFunction: FrontProtocol.DrawFunction = FrontProtocol.DrawGameWait
  val delay: Int = if (mode == 2) 2 else 1

  var pingMap = Map.empty[Short, Long] // id, 时间戳
  var isFirstTotalDataSync = false

  var pingId: Short = 0

  var frameTemp = 0

  var pingTimer = -1

  var gameLoopTimer = -1

  var renderId = 0

  private var offset = 0L

  private var score = Score("","","",0)
  private var winnerScore = Score("","","",0)

  private var recallFrame: scala.Option[Int] = None

  val idGenerator = new AtomicInteger(1)
  private var myActionHistory = Map[Int, (Int, Int)]() //(actionId, (keyCode, frameCount))
  private[this] val canvas = dom.document.getElementById("GameView").asInstanceOf[Canvas]
  private[this] val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  //  private[this] val audio1 = dom.document.getElementById("audio").asInstanceOf[HTMLAudioElement]
  private[this] val audioFinish = dom.document.getElementById("audioFinish").asInstanceOf[HTMLAudioElement]
  private[this] val audioKill = dom.document.getElementById("audioKill").asInstanceOf[HTMLAudioElement]
  private[this] val audioKilled = dom.document.getElementById("audioKilled").asInstanceOf[HTMLAudioElement]
  //  private[this] val bgm4 = dom.document.getElementById("bgm4").asInstanceOf[HTMLAudioElement]
  //  private[this] val bgmList = List(bgm4)
  //  private val bgmAmount = bgmList.length
  //  private var BGM = dom.document.getElementById("bgm4").asInstanceOf[HTMLAudioElement]
  private[this] val rankCanvas = dom.document.getElementById("RankView").asInstanceOf[Canvas] //把排行榜的canvas置于最上层，所以监听最上层的canvas
  private val x = (dom.window.innerWidth / 2).toInt - 145
  private val y = (dom.window.innerHeight / 2).toInt - 180
  private val myKeySet = mutable.HashSet[Int]()


  dom.document.addEventListener("visibilitychange", { e: Event =>
    if (dom.document.visibilityState.asInstanceOf[VisibilityState] != VisibilityState.hidden) {
      println("has Synced")
      updateListener()
    }
  })

  private var logicFrameTime = System.currentTimeMillis()

  private[this] val drawGame: DrawGame = new DrawGame(ctx, canvas, img)
  private[this] val webSocketClient: WebSocketClient = new WebSocketClient(connectOpenSuccess, connectError, messageHandler, connectClose)

  def init(): Unit = {
    val para = webSocketPara.asInstanceOf[PlayGamePara]
    addSession(para.playerId)
    webSocketClient.setUp(order, webSocketPara)
  }

  def updateListener(): Unit = {
    webSocketClient.sendMessage(NeedToSync.asInstanceOf[UserAction])
  }

  def getRandom(s: Int): Int = {
    val rnd = new scala.util.Random
    rnd.nextInt(s)
  }

  def startGame(): Unit = {
    drawGame.drawGameOn()
    //    BGM = bgmList(getRandom(bgmAmount))
    //    BGM.play()
    //    isPlay = true
    gameLoopTimer = dom.window.setInterval(() => gameLoop(), frameRate)
    pingTimer = dom.window.setInterval(() => {
      if (pingId > 10000) pingId = 0 else pingId = (pingId + 1).toShort
      pingMap += (pingId -> System.currentTimeMillis())
      webSocketClient.sendMessage(SendPingPacket(pingId).asInstanceOf[UserAction])
    }, 250)
    dom.window.requestAnimationFrame(gameRender())
  }

//  var lastTime1 = 0l
  def gameRender(): Double => Unit = { _ =>
    val curTime = System.currentTimeMillis()
//    println(s"requestAnimationTime: ${curTime - lastTime1}")
    val offsetTime = curTime - logicFrameTime
    offset = offsetTime
    draw(offsetTime)
//    lastTime1 = curTime
    if (isContinue)
      renderId = dom.window.requestAnimationFrame(gameRender())
  }


  def gameLoop(): Unit = {
    logicFrameTime = System.currentTimeMillis()
    if ((oldWindowBoundary.x != dom.window.innerWidth.toFloat) || (oldWindowBoundary.y != dom.window.innerHeight.toFloat)) {
      drawGame.resetScreen()
      oldWindowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
      if (!isContinue) {
        if (isWin) {
          drawGame.drawGameWin(myId, this.score, winnerScore)
        } else {
          drawGame.drawGameDie(killerInfo, score)
        }
      }
    }

    var isAlreadySendSync = false

    if (webSocketClient.getWsState) {
      recallFrame match {
        case Some(-1) =>
          println("!!!!!!!!:NeedToSync")
          webSocketClient.sendMessage(NeedToSync.asInstanceOf[UserAction])
          isAlreadySendSync = true
          recallFrame = None

        case Some(frame) =>
          val time1 = System.currentTimeMillis()
          println(s"before recall...frame:${grid.frameCount}")
          grid.historyDieSnake.filter { d => d._2.contains(myId) && d._1 > frame }.keys.headOption match {
            case Some(dieFrame) =>
              if (dieFrame - 2 > frame) grid.recallGrid(frame, dieFrame - 2)
              else grid.setGridInGivenFrame(frame)

            case None =>
              grid.recallGrid(frame, grid.frameCount)
          }
          println(s"after recall time: ${System.currentTimeMillis() - time1}...after frame:${grid.frameCount}")
          recallFrame = None

        case None =>
      }

      if (syncGridData.nonEmpty) { //全量数据
//        println(s"!!!!!!!!data sync:grid.frame${grid.frameCount}, syncFrame: ${syncGridData.get.frameCount}")
//        if (grid.boardMap.nonEmpty) {
////          println("total syncGridData")
//          grid.hsMap += grid.frameCount -> (grid.snakes, grid.grid, grid.snakeTurnPoints)
//          grid.historyStateMap += syncGridData.get.frameCount -> syncGridData.get
//        }
//        grid.yubelMap = grid.boardMap.map(s => s._2.yubelId -> s._1)
//        grid.addAllData(grid.historyStateMap.getOrElse(grid.frameCount, grid.getAllData))
        grid.addAllData(syncGridData.get)
        grid.updateBoardOnClient()
//        println(s"init finish: ${grid.frameCount}")
//        addBackendInfo4Sync(grid.frameCount)
        syncGridData = None
      } else if (syncFrame.nonEmpty ) { //局部数据仅同步帧号
//        println(s"========checkFrame:${syncFrame.get.frameCount}!")
        val frontend = grid.frameCount
        val backend = syncFrame.get.frameCount
        val advancedFrame = backend - frontend
        if (advancedFrame == 1) {
//          println(s"backend advanced frontend,frontend$frontend,backend:$backend")
//          grid.updateOnClient()
          grid.updateBoardOnClient()
//          addBackendInfo(grid.frameCount)
        } else if (advancedFrame < 0 && grid.historyStateMap.get(backend).nonEmpty) {
          println(s"frontend advanced backend,frontend$frontend,backend:$backend")
//          grid.setGridInGivenFrame(backend)
        } else if (advancedFrame == 0) {
          println(s"frontend equal to backend,frontend$frontend,backend:$backend")
        } else if (advancedFrame > 0 && advancedFrame < (grid.maxDelayed - 1)) {
          println(s"backend advanced frontend,frontend$frontend,backend:$backend")
          val endFrame = grid.historyDieBoard.filter { d => d._2.contains(myId) && d._1 > frontend }.keys.headOption match {
            case Some(dieFrame) => Math.min(dieFrame - 1, backend)
            case None => backend
          }
          (frontend until endFrame).foreach { _ =>
//            grid.updateOnClient()
            grid.updateBoardOnClient()
//            addBackendInfo(grid.frameCount)
          }
          println(s"after speed,frame:${grid.frameCount}")
        } else {
          println("neeeeeeeed help")
          if (!isAlreadySendSync) webSocketClient.sendMessage(NeedToSync.asInstanceOf[UserAction])
        }
        syncFrame = None
      } else {
//        grid.updateOnClient()
        grid.updateBoardOnClient()
//        addBackendInfo(grid.frameCount)
      }

      grid.historyDead.get(grid.frameCount) match {
        case Some(a) =>
          grid.boardMap -= myId
          grid.ballMap -= myId
        case _ =>
      }

//      println(s"Draw now what is $drawFunction!!!")

      if (!isWin) {

        drawFunction = grid.getAllData.boards.find(_._1 == myId) match {
          case Some(_) =>
            if (firstCome) firstCome = false
              FrontProtocol.DrawBaseGame(grid)

          case None if !firstCome =>
            FrontProtocol.DrawGameDie(killerInfo)

          case _ =>
            FrontProtocol.DrawGameWait
        }
      }
    } else {
      drawFunction = FrontProtocol.DrawGameOff
    }
  }

  def draw(offsetTime: Long): Unit = {

    drawFunction match {
      case FrontProtocol.DrawGameWait =>
        drawGame.drawGameWait()

      case FrontProtocol.DrawGameOff =>
        //        if(isPlay){
        //          BGM.pause()
        //          BGM.currentTime = 0
        //          isPlay = false
        //        }
        drawGame.drawGameOff(firstCome, None, false, false)

      case FrontProtocol.DrawGameWin(yourScore, winnerScore) =>

        drawGame.drawGameWin(myId, yourScore, winnerScore)
        isContinue = false

      case FrontProtocol.DrawBaseGame(data) =>
        drawGameImage(myId, data, offsetTime)

      case FrontProtocol.DrawGameDie(killerName, data) =>
//        if (data.nonEmpty) drawGameImage(myId, data.get, offsetTime)
        val s = if (grid.scoreMap.get(myId).isDefined) grid.scoreMap.get(myId).get else score
        drawGame.drawGameDie(killerName, s)
        isContinue = false
    }
  }


  def drawGameImage(uid: String, data: Grid, offsetTime: Long): Unit = {
    drawGame.drawBricks(grid.brickMap)
    drawGame.drawBoards(uid, offsetTime, grid)
    drawGame.drawBalls(uid, offsetTime, grid)
    drawGame.drawBoundary(grid)
  }

  private def connectOpenSuccess(event0: Event, order: String) = {
    if (order == "playGame") {
      startGame()
      rankCanvas.focus()
      rankCanvas.onkeydown = { e: dom.KeyboardEvent => {
        if (Constant.watchKeys.contains(e.keyCode) && !myKeySet.contains(e.keyCode)) {
          val frame = grid.frameCount + delay
          e.keyCode match {
            case KeyCode.Space =>
              drawFunction match {
                case FrontProtocol.DrawBaseGame(_) =>
                case FrontProtocol.DrawGameDie(_, _) =>
                  println("onkeydown:Space")
                  spaceKey()
                  val msg: Protocol.UserAction = PressSpace
                  webSocketClient.sendMessage(msg)
                case FrontProtocol.DrawGameWin(_,_) =>
                  println("onkeydown:Space")
                  grid.cleanData()
                  spaceKey()
                  val msg: Protocol.UserAction = PressSpace
                  webSocketClient.sendMessage(msg)
                case FrontProtocol.DrawGameOff if !firstCome=>
                  dom.window.cancelAnimationFrame(renderId)
                  killInfo = None
                  grid.actionMap = grid.actionMap.filterNot(_._2.contains(myId))
                  drawFunction = FrontProtocol.DrawGameWait
                  audioKilled.pause()
                  audioKilled.currentTime = 0
                  firstCome = true
                  isSynced = false
                  if (isWin) isWin = false
                  isContinue = true
                  val para = webSocketPara.asInstanceOf[PlayGamePara]
                  webSocketClient.setUp(order, ReJoinGamePara(para.playerId, para.playerName, para.mode, para.img))

                case _ =>
              }


            case _ =>
              drawFunction match {
                case _ =>
                  myKeySet.add(e.keyCode)
                  val newKeyCode =
                    if (mode == 1) {
                      val code = e.keyCode match {
                        case KeyCode.Left => KeyCode.Right
                        case KeyCode.Right => KeyCode.Left
                        case KeyCode.Down => KeyCode.Up
                        case KeyCode.Up => KeyCode.Down
                        case _ => KeyCode.Space
                      }
                      code.toByte
                    } else e.keyCode.toByte
                  val actionInfo = grid.getUserMaxActionFrame(myId, frame)
                  val actionId = idGenerator.getAndIncrement()
                  grid.addActionWithFrame(myId, newKeyCode, actionInfo._1, 0)
                  myActionHistory += actionId -> (newKeyCode, actionInfo._1)
                  val msgs: Protocol.UserAction = Keys(newKeyCode, actionInfo._1, actionId, 0)
                  webSocketClient.sendMessage(msgs)
//                  println("e: " + e.key)
//                  grid.addActionWithFrame(myId, newKeyCode, frame , 0)
                  if (actionInfo._1 < frame + maxContainableAction && actionInfo._2 != newKeyCode) {

                    println("e: " + e.key)
                  }
              }
          }
          e.preventDefault()
        }
      }
      }

      rankCanvas.onkeyup = { e: dom.KeyboardEvent =>
        if (Constant.watchKeys.contains(e.keyCode)) {
          val frame = grid.frameCount + delay
          e.keyCode match {
            case KeyCode.Space =>


            case _ =>
              myKeySet.remove(e.keyCode)
              val newKeyCode =
                if (mode == 1) {
                  val code = e.keyCode match {
                    case KeyCode.Left => KeyCode.Right
                    case KeyCode.Right => KeyCode.Left
                    case KeyCode.Down => KeyCode.Up
                    case KeyCode.Up => KeyCode.Down
                    case _ => KeyCode.Space
                  }
                  code.toByte
                } else e.keyCode.toByte


              val actionInfo = grid.getUserMaxActionFrame(myId, frame)
              grid.addActionWithFrame(myId, newKeyCode, frame, 1)
              val actionId = idGenerator.getAndIncrement()
              myActionHistory += actionId -> (newKeyCode, actionInfo._1)
              val msgs: Protocol.UserAction = Keys(newKeyCode, actionInfo._1, actionId, 1)
              webSocketClient.sendMessage(msgs)
//              grid.addActionWithFrame(myId, newKeyCode, frame, 1)
              if (actionInfo._1 < frame + maxContainableAction && actionInfo._2 != newKeyCode) {
                println("e: " + e.key)
              }
          }
          e.preventDefault()
        }

      }

    }
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
      case Protocol.Id(id) =>
        myId = id
        myTrueId = id

      case Protocol.CloseWs =>
        if (pingTimer != -1) {
          dom.window.clearInterval(pingTimer)
          pingTimer = -1
        }
        dom.window.clearInterval(gameLoopTimer)
        drawFunction = FrontProtocol.DrawGameOff


      case r@Protocol.BoardAction(yubelId, keyCode, frame, actionId, typ) =>
        if (grid.boardMap.contains(grid.yubelMap.getOrElse(yubelId, ""))) {
          val id = grid.yubelMap(yubelId)
          if (id == myId) { //收到自己的进行校验是否与预判一致，若不一致则回溯
            if (myActionHistory.get(actionId).isEmpty) { //前端没有该项，则加入
              grid.addActionWithFrame(id, keyCode, frame, typ)
              if (frame < grid.frameCount) {
                println(s"recall for my Action1,backend:$frame,frontend:${grid.frameCount}")
                recallFrame = grid.findRecallFrame(frame, recallFrame)
              }
            } else {
              if (myActionHistory(actionId)._1 != keyCode || myActionHistory(actionId)._2 != frame) { //若keyCode或frame不一致则进行回溯
                grid.deleteActionWithFrame(id, myActionHistory(actionId)._2)
                grid.addActionWithFrame(id, keyCode, frame, typ)
                val miniFrame = Math.min(frame, myActionHistory(actionId)._2)
                if (miniFrame < grid.frameCount) {
                  println(s"recall for my Action2,backend:$miniFrame,frontend:${grid.frameCount}")
                  recallFrame = grid.findRecallFrame(miniFrame, recallFrame)
                }
              }
              myActionHistory -= actionId
            }
          }
        }

      case m@OtherAction(carnieId, keyCode, frame, typ) =>
//        println(s"!!recv $m")
        if (grid.boardMap.contains(grid.yubelMap.getOrElse(carnieId, ""))) {
          val id = grid.yubelMap(carnieId)
          grid.addActionWithFrame(id, keyCode, frame, typ)
          if (frame < grid.frameCount) {
            println(s"recall for other Action,backend:$frame,frontend:${grid.frameCount}")
            recallFrame = grid.findRecallFrame(frame, recallFrame)
          }
        }

      case m@InitActions(actions) =>
        println(s"recv $m")
        actions.foreach(messageHandler(_))


      case Protocol.OthersVary(boards) =>
        boards.foreach{ b =>
          grid.boardMap -= b._1
          grid.boardMap += b
        }

      case UserLeft(id) =>
        println(s"user $id left:::")
        grid.yubelMap = grid.yubelMap.filterNot(_._2 == id)

        grid.cleanDiedBoardInfo(List(id))
        val field = grid.searchMyField(id)
        println(s"the left userId: $field")


      case Protocol.SyncFrame(f) =>
//        println(s"sync backend: $f, frontend: ${grid.frameCount}")
        syncFrame = Some(SyncFrame(f))


      case x@Protocol.ScoreData(score) =>
        grid.scoreMap = score
        if (grid.boardMap.exists(_._1 == myId) && !isWin && isContinue)
          drawGame.drawRank(myId, grid)


      case data: Protocol.AllData =>
        println(s"===========recv total data")
//        grid.addAllData(data)
        if (!isFirstTotalDataSync) {
          println("add all data")
          grid.addAllData(data) //立刻执行，不再等到逻辑帧
          isFirstTotalDataSync = true
        }
        else {
//          println(s"back: ${data.frameCount},front: ${grid.frameCount}")
          syncGridData = Some(data)
//          isSynced = true
        }

      case data: Protocol.NewBoard =>
        println(s"===========recv new board data ")
      //        grid.addDataExceptBall(data)
        grid.addData(data.allNew)

      case x@Protocol.DeadBoard(frame,score,playTime) =>
//        drawFunction = FrontProtocol.DrawGameDie(Some("sss"), Some(grid.getGridData4Draw(myId,1.0)))
        grid.historyDead += (frame -> (score,playTime))
        grid.boardMap -= myId
        grid.ballMap -= myId

      case x@Protocol.OthersDead(frame, id) =>
        //        drawFunction = FrontProtocol.DrawGameDie(Some("sss"), Some(grid.getGridData4Draw(myId,1.0)))
        id.foreach{ i =>
          grid.boardMap -= i
          grid.ballMap -= i
          grid.scoreMap -= i
        }
//      case x@Protocol.DeadPage(kill, area, playTime) =>
//        println(s"recv userDead $x")
//        myScore = BaseScore(kill, area, playTime)
//        maxArea = Constant.shortMax(maxArea, area)

      case x@Protocol.GameWin(s) =>
        isWin = true
        this.score = s.filter(_.id == myId).head
        this.winnerScore = s.sortBy(_.score).reverse.head
        isFirstTotalDataSync = false
        drawFunction = FrontProtocol.DrawGameWin(this.score, this.winnerScore)




      case x@Protocol.ReceivePingPacket(recvPingId) =>
        val currentTime = System.currentTimeMillis()
        if (pingMap.get(recvPingId).nonEmpty) {
          PerformanceTool.receivePingPackage(pingMap(recvPingId), currentTime)
          pingMap -= recvPingId
        }


      case x@_ =>
        println(s"receive unknown msg:$x")
    }
  }

  def spaceKey(): Unit = {
    grid.boardActionMap = grid.boardActionMap.filterNot(_._2.contains(myId))
    drawFunction = FrontProtocol.DrawGameWait
    //    audio1.pause()
    //    audio1.currentTime = 0
    audioKilled.pause()
    audioKilled.currentTime = 0
    firstCome = true
    isSynced = false
    if (isWin) isWin = false
    score = Score("","","",0)
    winnerScore = Score("","","",0)
    isContinue = true
    //                  backBtn.style.display="none"
    //                  rankCanvas.addEventListener("",null)
    dom.window.requestAnimationFrame(gameRender())
  }


  def addSession(id: String) = {
    val url = Routes.yubel.addSession+s"?id=$id"
    Http.getAndParse[SuccessRsp](url).map {
      case Right(rsp) =>
        if (rsp.errCode == 0) {
          println("Success to addSession!")
        }
        else {
          println(s"Some errors happened in addSession: ${rsp.msg}")
        }

      case Left(e) =>
        println(s"Some errors happened in addSession: $e")
    }

  }

  override def render: Elem = {
    init()
    <div></div>
  }
}
