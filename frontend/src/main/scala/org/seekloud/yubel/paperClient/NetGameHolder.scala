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

  private var myScore = BaseScore(0, 0, 0)
  private var maxArea: Short = 0
  private var winningData = WinData(0, Some(0), "")

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
//    addSession(para.playerId)
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
//          drawGame.drawGameWin(myId, grid.scoreMap)
        } else {
          drawGame.drawGameDie(killerInfo, myScore, maxArea)
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
        println(s"========checkFrame:${syncFrame.get.frameCount}!")
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



      if (!isWin) {
        drawFunction = grid.getAllData.boards.find(_._1 == myId) match {
          case Some(_) =>
            if (firstCome) firstCome = false
            //            if (!isPlay) {
            ////              println("you can't play")
            //              BGM = bgmList(getRandom(bgmAmount))
            //              BGM.play()
            //              isPlay = true
            //            }
            //            if (BGM.paused) {
            //              isPlay = false
            //            }
            if (myId == myTrueId) {
              isPlay = true
              FrontProtocol.DrawBaseGame(grid)
            }
            else
              FrontProtocol.DrawGameDie(killerInfo, Some(grid))

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
    //    println(s"drawFunction:::$drawFunction")
    drawFunction match {
      case FrontProtocol.DrawGameWait =>
        //        println(s"drawFunction::: drawGameWait")
        drawGame.drawGameWait()
//        drawGameImage(myId,grid.getGridData4Draw(myId,1),offsetTime)

      case FrontProtocol.DrawGameOff =>
        //        println(s"drawFunction::: drawGameOff")
        //        if(isPlay){
        //          BGM.pause()
        //          BGM.currentTime = 0
        //          isPlay = false
        //        }
        drawGame.drawGameOff(firstCome, None, false, false)

      case FrontProtocol.DrawGameWin(score) =>
        drawGame.drawGameWin(myId, score)
        isContinue = false

      case FrontProtocol.DrawBaseGame(data) =>
        //        println(s"draw---DrawBaseGame!! snakes:${data.snakes.map(_.id)}")
        drawGameImage(myId, data, offsetTime)

      case FrontProtocol.DrawGameDie(killerName, data) =>
        //        if(isPlay){
        //          BGM.pause()
        //          BGM.currentTime = 0
        //          isPlay = false
        //        }
//        if (isPlay) {
//          audioKilled.play()
//          isPlay = false
//        }
//        if (data.nonEmpty) drawGameImage(myId, data.get, offsetTime)
        drawGame.drawGameDie(killerName, myScore, maxArea)
        killInfo = None
//        isContinue = false
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
                  isGetKiller = false
                  killerInfo = None
                  val msg: Protocol.UserAction = PressSpace
                  webSocketClient.sendMessage(msg)
                case FrontProtocol.DrawGameWin(_) =>
                  println("onkeydown:Space")
                  val msg: Protocol.UserAction = PressSpace
                  webSocketClient.sendMessage(msg)
                case FrontProtocol.DrawGameOff if !firstCome=>
                  dom.window.cancelAnimationFrame(renderId)
                  killInfo = None
                  grid.actionMap = grid.actionMap.filterNot(_._2.contains(myId))
                  drawFunction = FrontProtocol.DrawGameWait
                  //    audio1.pause()
                  //    audio1.currentTime = 0
                  audioKilled.pause()
                  audioKilled.currentTime = 0
                  firstCome = true
                  isSynced = false
                  isGetKiller = false
                  killerInfo = None
                  if (isWin) isWin = false
                  myScore = BaseScore(0, 0, 0)
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
//                  e.keyCode match {
//                    case KeyCode.A => drawGame.drawEmotion(myId, offset, grid,1)
//                    case KeyCode.S => drawGame.drawEmotion(myId, offset, grid,2)
//                    case KeyCode.D => drawGame.drawEmotion(myId, offset, grid,3)
//                    case _ =>
//                  }
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

      case UserLeft(id) =>
        println(s"user $id left:::")
        grid.yubelMap = grid.yubelMap.filterNot(_._2 == id)

        grid.cleanDiedBoardInfo(List(id))
        val field = grid.searchMyField(id)
        println(s"the left userId: $field")

      case Protocol.NewData(frameCount, newSnakes, newField) =>
        if (newSnakes.isDefined) {
//          println(s"get newSnakes!!!")
          val data = newSnakes.get
          data.snake.foreach { s => grid.yubelMap += s.yubelId -> s.id }
          grid.historyNewSnake += frameCount -> (data.snake, data.filedDetails.map { f =>
            FieldByColumn(grid.yubelMap.getOrElse(f.uid, ""), f.scanField)
          })
          if(frameCount == grid.frameCount){
            addNewSnake(frameCount)
          } else if (frameCount < grid.frameCount) {
            println(s"recall for NewSnakeInfo,backend:$frameCount,frontend:${grid.frameCount}")
            recallFrame = grid.findRecallFrame(frameCount - 1, recallFrame)
          }
        }
        if (newField.isDefined) {
//          println(s"get newFields!!!")
          val fields = newField.get.map{f =>
            if(grid.yubelMap.get(f.uid).isEmpty) println(s"!!!!!!!error:::can not find id: ${f.uid} from carnieMap")
            FieldByColumn(grid.yubelMap.getOrElse(f.uid, ""), f.scanField)}
          if (fields.exists(_.uid == myTrueId)) {
            audioFinish.play()
            val myField = fields.filter(_.uid == myTrueId)
//            println(s"fieldDetail: $myField")
          }
          grid.historyFieldInfo += frameCount -> fields
          if(frameCount == grid.frameCount){
            addFieldInfo(frameCount)
          } else if (frameCount < grid.frameCount) {
            println(s"recall for NewFieldInfo,backend:$frameCount,frontend:${grid.frameCount}")
            recallFrame = grid.findRecallFrame(frameCount - 1, recallFrame)
          }
        }

//        syncFrame = Some(SyncFrame(frameCount))
        isSynced = true

      case Protocol.SyncFrame(f) =>
//        println(s"sync backend: $f, frontend: ${grid.frameCount}")
        syncFrame = Some(SyncFrame(f))


      case x@Protocol.ScoreData(score) =>
        grid.scoreMap = score
        if (grid.boardMap.exists(_._1 == myId) && !isWin && isContinue)
          drawGame.drawRank(myId, grid)

//      case Protocol.Ranks(ranks, personalScore, personalRank, currentNum) =>
//        currentRank = ranks
//        if (grid.snakes.exists(_._1 == myId) && !isWin && isContinue)
//          drawGame.drawRank(myId, grid.snakes.values.toList, currentRank, personalScore, personalRank, currentNum)

//      case data: Protocol.Data4TotalSync =>
//        println(s"===========recv total data")
//        if (!isFirstTotalDataSync) {
//          grid.initSyncGridData(data) //立刻执行，不再等到逻辑帧
//          isFirstTotalDataSync = true
//        } else {
//          syncGridData = Some(data)
//          isSynced = true
//        }

      case data: Protocol.AllData =>
        println(s"===========recv total data")
//        grid.addAllData(data)
        if (!isFirstTotalDataSync) {
          grid.addAllData(data) //立刻执行，不再等到逻辑帧
          isFirstTotalDataSync = true
        }
        else {
//          println(s"back: ${data.frameCount},front: ${grid.frameCount}")
          syncGridData = Some(data)
//          isSynced = true
        }

      case data: Protocol.NewBoard =>
        println(s"===========recv data except brick")
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

      case x@Protocol.GameWin(score) =>
        drawFunction = FrontProtocol.DrawGameWin(score)
        grid.cleanData()

//      case Protocol.UserDeadMsg(frame, deadInfo) =>
//        val deadList = deadInfo.map(baseInfo => grid.carnieMap.getOrElse(baseInfo.carnieId, ""))
//        println(s"=======deadList:$deadList")
//        deadInfo.find{d => grid.carnieMap.getOrElse(d.carnieId, "") == myId} match {
//          case Some(myKillInfo) if myKillInfo.killerId.nonEmpty =>
//            val info = grid.snakes.get(grid.carnieMap.getOrElse(myKillInfo.killerId.get, ""))
//            if (myId == myTrueId) { //死亡观战视角时，不更新killInfo
//              isGetKiller = true
//              killerInfo = info.map(_.name)
//            }
//            if (info.map(_.id).nonEmpty && !deadList.contains(info.get.id)) myId = info.get.id
//            else if(currentRank.filterNot(_.id == myId).nonEmpty) myId = currentRank.filterNot(_.id == myId).head.id
//
//          case None =>
//
//          case _ =>
//            if (myId == myTrueId) {
//              isGetKiller = true
//              killerInfo = None
//            }
//            if(currentRank.filterNot(_.id == myId).nonEmpty) myId = currentRank.filterNot(_.id == myId).head.id
//        }
//
//        grid.historyDieSnake += frame -> deadList
//        deadInfo.filter(_.killerId.nonEmpty).foreach { i =>
//          val idOp = grid.carnieMap.get(i.carnieId)
//          if (idOp.nonEmpty) {
//            val id = idOp.get
//            val name = grid.snakes.get(id).map(_.name).getOrElse("unknown")
//            val killerId = grid.carnieMap.getOrElse(i.killerId.get, "")
//            val killerName = grid.snakes.get(killerId).map(_.name).getOrElse("unknown")
//            killInfo = Some(id, name, killerName, killerId)
//            barrageDuration = 100
//          }
//        }
//        if(frame == grid.frameCount){
//          println(s"!!!!!!!!!!!!!!frame==grid.frame")
//          addDieSnake(frame)
//        } else if (frame < grid.frameCount) {
//          println(s"recall for UserDeadMsg,backend:$frame,frontend:${grid.frameCount}")
//          val deadRecallFrame = if (deadList.contains(myId)) frame - 2 else frame - 1
//          recallFrame = grid.findRecallFrame(deadRecallFrame, recallFrame)
//        }



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
    grid.cleanSnakeTurnPoint(myId)
    killInfo = None
    grid.boardActionMap = grid.boardActionMap.filterNot(_._2.contains(myId))
    drawFunction = FrontProtocol.DrawGameWait
    //    audio1.pause()
    //    audio1.currentTime = 0
    audioKilled.pause()
    audioKilled.currentTime = 0
    firstCome = true
    isSynced = false
    isGetKiller = false
    killerInfo = None
    if (isWin) isWin = false
    myScore = BaseScore(0, 0, 0)
    isContinue = true
    //                  backBtn.style.display="none"
    //                  rankCanvas.addEventListener("",null)
    dom.window.requestAnimationFrame(gameRender())
  }

  def addBackendInfo(frame: Int): Unit = {
    addDieSnake(frame)
    addNewSnake(frame)
    addFieldInfo(frame)
  }

  def addFieldInfo(frame: Int): Unit = {
    grid.historyFieldInfo.get(frame).foreach { data =>
//      if (data.nonEmpty) println(s"addFieldInfo:$frame")
      grid.addNewFieldInfo(data)
    }
  }

  def addDieSnake(frame: Int): Unit = {
    grid.historyDieSnake.get(frame).foreach { deadSnake =>
      grid.cleanDiedBoardInfo(deadSnake)
    }
  }

  def addNewSnake(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
      if (newSnakes._1.map(_.id).contains(myId) && !firstCome && !isContinue) spaceKey()
      newSnakes._1.foreach { s => grid.cleanSnakeTurnPoint(s.id) } //清理死前拐点
      grid.snakes ++= newSnakes._1.map(s => s.id -> s).toMap
      grid.addNewFieldInfo(newSnakes._2)
      if (newSnakes._1.exists(_.id == myTrueId)) myId = myTrueId

    }
  }

  def addBackendInfo4Sync(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
      if (newSnakes._1.map(_.id).contains(myId) && !firstCome && !isContinue) spaceKey()
    }
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
