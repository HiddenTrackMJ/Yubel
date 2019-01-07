package com.neo.sk.carnie.paperClient

import java.util.concurrent.atomic.AtomicInteger
import com.neo.sk.carnie.common.Constant
import org.scalajs.dom.html.Canvas
import com.neo.sk.carnie.paperClient.Protocol._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html.{Document => _, _}
import org.scalajs.dom.raw._
import com.neo.sk.carnie.paperClient.WebSocketProtocol._
import com.neo.sk.carnie.util.Component

import scala.xml.Elem

/**
  * User: Tao
  * Date: 9/1/2016
  * Time: 12:45 PM
  */

class NetGameHolder(order: String, webSocketPara: WebSocketPara, mode: Int, img: Int = 0, frameRate: Int = 150) extends Component {
  //0:正常模式，1:反转模式, 2:2倍加速模式

  var currentRank = List.empty[Score]
  var historyRank = List.empty[Score]
  private var myId = ""

  var grid = new GridOnClient(Point(BorderSize.w, BorderSize.h))

  var isGetKiller = false
  var killerInfo: scala.Option[String] = None
  var firstCome = true
  var isSynced = false
  //  var justSynced = false
  var isWin = false
  var isPlay = false
  //  var winnerName = "unknown"
  var killInfo: scala.Option[(String, String, String)] = None
  var barrageDuration = 0

  var syncFrame: scala.Option[Protocol.SyncFrame] = None
  var syncGridData: scala.Option[Protocol.Data4TotalSync] = None
  var isContinue = true
  var oldWindowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
  var drawFunction: FrontProtocol.DrawFunction = FrontProtocol.DrawGameWait
  val delay: Int = if (mode == 2) 2 else 1

  var pingMap = Map.empty[Short, Long] // id, 时间戳

  var pingId: Short = 0

  private var myScore = BaseScore(0, 0, 0)
  private var maxArea: Short = 0
  private var winningData = WinData(0, Some(0))

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


  dom.document.addEventListener("visibilitychange", { e: Event =>
    if (dom.document.visibilityState.asInstanceOf[VisibilityState] != VisibilityState.hidden) {
      println("has Synced")
      updateListener()
    }
  })

  private var logicFrameTime = System.currentTimeMillis()

  private[this] val drawGame: DrawGame = new DrawGame(ctx, canvas, img)
  private[this] val webSocketClient: WebSocketClient = new WebSocketClient(connectOpenSuccess, connectError, messageHandler, connectError)

  def init(): Unit = {
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
    dom.window.setInterval(() => gameLoop(), frameRate)
    dom.window.setInterval(() => {
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
    draw(offsetTime)
//    lastTime1 = curTime
    if (isContinue)
      dom.window.requestAnimationFrame(gameRender())
  }


  def gameLoop(): Unit = {
    logicFrameTime = System.currentTimeMillis()
    if ((oldWindowBoundary.x != dom.window.innerWidth.toFloat) || (oldWindowBoundary.y != dom.window.innerHeight.toFloat)) {
      drawGame.resetScreen()
      oldWindowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
      if (!isContinue) {
        if (isWin) {
          val winInfo = drawFunction.asInstanceOf[FrontProtocol.DrawGameWin]
          drawGame.drawGameWin(myId, winInfo.winnerName, winInfo.winData, winningData)
        } else {
          drawGame.drawGameDie(grid.getKiller(myId).map(_._2), myScore, maxArea)
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
        if (grid.snakes.nonEmpty) {
          println("total syncGridData")
          grid.historyStateMap += grid.frameCount -> (grid.snakes, grid.grid, grid.snakeTurnPoints)
        }
        grid.initSyncGridData(syncGridData.get)
        addBackendInfo4Sync(grid.frameCount)
        syncGridData = None
      } else if (syncFrame.nonEmpty) { //局部数据仅同步帧号
        val frontend = grid.frameCount
        val backend = syncFrame.get.frameCount
        val advancedFrame = backend - frontend
        if (advancedFrame == 1) {
          println(s"backend advanced frontend,frontend$frontend,backend:$backend")
          grid.updateOnClient()
          addBackendInfo(grid.frameCount)
        } else if (advancedFrame < 0 && grid.historyStateMap.get(backend).nonEmpty) {
          println(s"frontend advanced backend,frontend$frontend,backend:$backend")
          grid.setGridInGivenFrame(backend)
        } else if (advancedFrame == 0) {
          println(s"frontend equal to backend,frontend$frontend,backend:$backend")
        } else if (advancedFrame > 0 && advancedFrame < (grid.maxDelayed - 1)) {
          println(s"backend advanced frontend,frontend$frontend,backend:$backend")
          val endFrame = grid.historyDieSnake.filter { d => d._2.contains(myId) && d._1 > frontend }.keys.headOption match {
            case Some(dieFrame) => Math.min(dieFrame - 1, backend)
            case None => backend
          }
          (frontend until endFrame).foreach { _ =>
            grid.updateOnClient()
            addBackendInfo(grid.frameCount)
          }
          println(s"after speed,frame:${grid.frameCount}")
        } else {
          if (!isAlreadySendSync) webSocketClient.sendMessage(NeedToSync.asInstanceOf[UserAction])
        }
        syncFrame = None
      } else {
        grid.updateOnClient()
        addBackendInfo(grid.frameCount)
      }

      if (!isWin) {
        val gridData = grid.getGridData4Draw
        drawFunction = gridData.snakes.find(_.id == myId) match {
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
            FrontProtocol.DrawBaseGame(gridData)

          case None if isGetKiller && !firstCome =>
            FrontProtocol.DrawGameDie(killerInfo)

          case _ =>
            FrontProtocol.DrawGameWait
        }
      }
    } else {
      drawFunction = FrontProtocol.DrawGameOff
    }
    val dealTime = System.currentTimeMillis() - logicFrameTime
    if (dealTime > 50)
      println(s"logicFrame deal time:$dealTime")
  }

  def draw(offsetTime: Long): Unit = {
    //    println(s"drawFunction:::$drawFunction")
    drawFunction match {
      case FrontProtocol.DrawGameWait =>
        //        println(s"drawFunction::: drawGameWait")
        drawGame.drawGameWait()

      case FrontProtocol.DrawGameOff =>
        //        println(s"drawFunction::: drawGameOff")
        //        if(isPlay){
        //          BGM.pause()
        //          BGM.currentTime = 0
        //          isPlay = false
        //        }
        drawGame.drawGameOff(firstCome, None, false, false)

      case FrontProtocol.DrawGameWin(winner, winData) =>
        //        if(isPlay){
        //          BGM.pause()
        //          BGM.currentTime = 0
        //          isPlay = false
        //        }
        drawGame.drawGameWin(myId, winner, winData, winningData)
        //        audio1.play()
        isContinue = false

      case FrontProtocol.DrawBaseGame(data) =>
        //        println(s"draw---DrawBaseGame!! snakes:${data.snakes.map(_.id)}")
        drawGameImage(myId, data, offsetTime)
        if (killInfo.nonEmpty) {
          val killBaseInfo = killInfo.get
          if (killBaseInfo._3 == myId) audioKill.play()
          drawGame.drawBarrage(killBaseInfo._2, killBaseInfo._3)
          barrageDuration -= 1
          if (barrageDuration == 0) killInfo = None
        }

      case FrontProtocol.DrawGameDie(killerName) =>
        //        println(s"drawFunction::: drawGameDie")
        //        if(isPlay){
        //          BGM.pause()
        //          BGM.currentTime = 0
        //          isPlay = false
        //        }
        if (isContinue) audioKilled.play()
        drawGame.drawGameDie(killerName, myScore, maxArea)
        killInfo = None
        isContinue = false
    }
  }


  def drawGameImage(uid: String, data: FrontProtocol.Data4Draw, offsetTime: Long): Unit = {
    drawGame.drawGrid(uid, data, offsetTime, grid, currentRank.headOption.map(_.id).getOrElse(myId),
      frameRate = frameRate, newFieldInfo = grid.historyFieldInfo.get(grid.frameCount + 1))
    drawGame.drawSmallMap(data.snakes.filter(_.id == uid).map(_.header).head, data.snakes.filterNot(_.id == uid))
  }

  private def connectOpenSuccess(event0: Event, order: String) = {
    if (order == "playGame") {
      startGame()
      rankCanvas.focus()
      rankCanvas.onkeydown = { e: dom.KeyboardEvent => {
        if (Constant.watchKeys.contains(e.keyCode)) {
          val frame = grid.frameCount + delay
          e.keyCode match {
            case KeyCode.Space =>
              drawFunction match {
                case FrontProtocol.DrawBaseGame(_) =>
                case _ =>
                  println("onkeydown:Space")
                  isGetKiller = false
                  killerInfo = None
                  val msg: Protocol.UserAction = PressSpace
                  webSocketClient.sendMessage(msg)
              }


            case _ =>
              drawFunction match {
                case FrontProtocol.DrawBaseGame(_) =>
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
                  if (actionInfo._1 < frame + maxContainableAction && actionInfo._2 != newKeyCode) {
                    val actionId = idGenerator.getAndIncrement()
                    grid.addActionWithFrame(myId, newKeyCode, actionInfo._1)
                    myActionHistory += actionId -> (newKeyCode, actionInfo._1)
                    val msg: Protocol.UserAction = Key(newKeyCode, actionInfo._1, actionId)
                    webSocketClient.sendMessage(msg)
                  }
                case _ =>
              }
          }
          e.preventDefault()
        }
      }
      }

//      rankCanvas.onmouseup = { e: dom.MouseEvent =>
//        val myField = grid.grid.filter(_._2 == Field(myId))
//        val myBody = grid.snakeTurnPoints.getOrElse(myId, Nil)
//
//
//        val myGroupField = FieldByColumn(myId, myField.keys.groupBy(_.y).map { case (y, target) =>
//          (y.toShort, Tool.findContinuous(target.map(_.x.toShort).toArray.sorted)) //read
//        }.toList.groupBy(_._2).map { case (r, target) =>
//          ScanByColumn(Tool.findContinuous(target.map(_._1).toArray.sorted), r)
//        }.toList)
//
//
//        println(s"=======myField:$myGroupField, myBody:$myBody")
//      }

      //退出房间触发事件
      //      rankCanvas.onmousedown = { e:dom.MouseEvent =>
      //        drawFunction match {
      //          case FrontProtocol.DrawGameDie(_) =>
      //            if(
      //              e.pageX > x &&
      //                e.pageX < x + 175 &&
      //                e.pageY > y + 250 &&
      //                e.pageY < y + 310
      //            ) {
      //              dom.document.location.reload() //重新进入游戏
      ////              dom.window.location.reload()
      //            }
      //          case _ =>
      //        }
      //        e.preventDefault()
      //      }
    }
    event0
  }

  private def connectError(e: Event) = {
    drawGame.drawGameOff(firstCome, None, false, false)
    e
  }

  private def messageHandler(data: GameMessage): Unit = {
    data match {
      case Protocol.Id(id) =>
        myId = id

      case r@Protocol.SnakeAction(carnieId, keyCode, frame, actionId) =>
        if (grid.snakes.contains(grid.carnieMap.getOrElse(carnieId, ""))) {
          val id = grid.carnieMap(carnieId)
          if (id == myId) { //收到自己的进行校验是否与预判一致，若不一致则回溯
            if (myActionHistory.get(actionId).isEmpty) { //前端没有该项，则加入
              grid.addActionWithFrame(id, keyCode, frame)
              if (frame < grid.frameCount) {
                println(s"recall for my Action1,backend:$frame,frontend:${grid.frameCount}")
                recallFrame = grid.findRecallFrame(frame, recallFrame)
              }
            } else {
              if (myActionHistory(actionId)._1 != keyCode || myActionHistory(actionId)._2 != frame) { //若keyCode或则frame不一致则进行回溯
                grid.deleteActionWithFrame(id, myActionHistory(actionId)._2)
                grid.addActionWithFrame(id, keyCode, frame)
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

      case OtherAction(carnieId, keyCode, frame) =>
        if (grid.snakes.contains(grid.carnieMap.getOrElse(carnieId, ""))) {
          val id = grid.carnieMap(carnieId)
          grid.addActionWithFrame(id, keyCode, frame)
          if (frame < grid.frameCount) {
            println(s"recall for other Action,backend:$frame,frontend:${grid.frameCount}")
            recallFrame = grid.findRecallFrame(frame, recallFrame)
          }
        }

      case UserLeft(id) =>
        println(s"user $id left:::")
        grid.carnieMap = grid.carnieMap.filterNot(_._2 == id)
        grid.cleanDiedSnakeInfo(id)

      case Protocol.SomeOneWin(winner) =>
        drawFunction = FrontProtocol.DrawGameWin(winner, grid.getGridData)
        isWin = true
        //        winnerName = winner
        //        winData = finalData
        grid.cleanData()

      case Protocol.WinnerBestScore(score) =>
        maxArea = Constant.shortMax(maxArea, score)

      case Protocol.Ranks(ranks, personalScore, personalRank, currentNum) =>
        currentRank = ranks
        if (grid.getGridData.snakes.exists(_.id == myId) && !isWin && isContinue)
          drawGame.drawRank(myId, grid.getGridData.snakes, currentRank, personalScore, personalRank, currentNum)

      case data: Protocol.Data4TotalSync =>
        println(s"===========recv total data")
        syncGridData = Some(data)
        isSynced = true

      case data: Protocol.SyncFrame =>
        syncFrame = Some(data)
        isSynced = true

      case data: Protocol.NewSnakeInfo =>
        data.snake.foreach { s => grid.carnieMap += s.carnieId -> s.id }
        grid.historyNewSnake += data.frameCount -> (data.snake, data.filedDetails.map { f =>
          FieldByColumn(grid.carnieMap.getOrElse(f.uid, ""), f.scanField)
        })
        if(data.frameCount == grid.frameCount){
          addNewSnake(data.frameCount)
        } else if (data.frameCount < grid.frameCount) {
          println(s"recall for NewSnakeInfo,backend:${data.frameCount},frontend:${grid.frameCount}")
          recallFrame = grid.findRecallFrame(data.frameCount - 1, recallFrame)
        }

      case x@Protocol.DeadPage(kill, area, playTime) =>
        println(s"recv userDead $x")
        myScore = BaseScore(kill, area, playTime)
        maxArea = Constant.shortMax(maxArea, area)

      case Protocol.UserDeadMsg(frame, deadInfo) =>
        deadInfo.find{d => grid.carnieMap.getOrElse(d.carnieId, "") == myId} match {
          case Some(myKillInfo) if myKillInfo.killerId.nonEmpty =>
            isGetKiller = true
            killerInfo = grid.snakes.get(grid.carnieMap.getOrElse(myKillInfo.killerId.get, "")).map(_.name)

          case None =>

          case _ =>
            isGetKiller = true
            killerInfo = None
        }
        val deadList = deadInfo.map(baseInfo => grid.carnieMap.getOrElse(baseInfo.carnieId, ""))
        grid.historyDieSnake += frame -> deadList
        deadInfo.filter(_.killerId.nonEmpty).foreach { i =>
          val idOp = grid.carnieMap.get(i.carnieId)
          if (idOp.nonEmpty) {
            val id = idOp.get
            val name = grid.snakes.get(id).map(_.name).getOrElse("unknown")
            val killerName = grid.snakes.get(grid.carnieMap.getOrElse(i.killerId.get, "")).map(_.name).getOrElse("unknown")
            killInfo = Some(id, name, killerName)
            barrageDuration = 100
          }
        }
        if(frame == grid.frameCount){
          addDieSnake(frame)
        } else if (frame < grid.frameCount) {
          println(s"recall for UserDeadMsg,backend:$frame,frontend:${grid.frameCount}")
          val deadRecallFrame = if (deadList.contains(myId)) frame - 2 else frame - 1
          recallFrame = grid.findRecallFrame(deadRecallFrame, recallFrame)
        }

      case data: Protocol.NewFieldInfo =>
        val fields = data.fieldDetails.map{f =>FieldByColumn(grid.carnieMap.getOrElse(f.uid, ""), f.scanField)}
        if (fields.exists(_.uid == myId)) audioFinish.play()
        grid.historyFieldInfo += data.frameCount -> fields
        if(data.frameCount == grid.frameCount){
          addFieldInfo(data.frameCount)
        } else if (data.frameCount < grid.frameCount) {
          println(s"recall for NewFieldInfo,backend:${data.frameCount},frontend:${grid.frameCount}")
          recallFrame = grid.findRecallFrame(data.frameCount - 1, recallFrame)
        }

      case x@Protocol.ReceivePingPacket(recvPingId) =>
        val currentTime = System.currentTimeMillis()
        if (pingMap.get(recvPingId).nonEmpty) {
          PerformanceTool.receivePingPackage(pingMap(recvPingId), currentTime)
          pingMap -= recvPingId
        }

      case x@Protocol.WinData(_, _) =>
        println(s"receive winningData msg:$x")
        winningData = x

      case x@_ =>
        println(s"receive unknown msg:$x")
    }
  }

  def spaceKey(): Unit = {
    grid.cleanSnakeTurnPoint(myId)
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
    //                  backBtn.style.display="none"
    //                  rankCanvas.addEventListener("",null)
    dom.window.requestAnimationFrame(gameRender())
  }

  def addBackendInfo(frame: Int): Unit = {
    addFieldInfo(frame)
    addDieSnake(frame)
    addNewSnake(frame)
  }

  def addFieldInfo(frame: Int): Unit = {
    grid.historyFieldInfo.get(frame).foreach { data =>
      grid.addNewFieldInfo(data)
    }
  }

  def addDieSnake(frame: Int): Unit = {
    grid.historyDieSnake.get(frame).foreach { deadSnake =>
      deadSnake.foreach { sid =>
        grid.cleanDiedSnakeInfo(sid)
      }
    }
  }

  def addNewSnake(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
      if (newSnakes._1.map(_.id).contains(myId) && !firstCome && !isContinue) spaceKey()
      newSnakes._1.foreach { s => grid.cleanSnakeTurnPoint(s.id) } //清理死前拐点
      grid.snakes ++= newSnakes._1.map(s => s.id -> s).toMap
      grid.addNewFieldInfo(newSnakes._2)
    }
  }

  def addBackendInfo4Sync(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
      if (newSnakes._1.map(_.id).contains(myId) && !firstCome && !isContinue) spaceKey()
    }
  }

  private var myGroupField: FieldByColumn = FieldByColumn(myId, Nil)

  private def getMyField(): Unit = {
    val myField = grid.grid.filter(_._2 == Field(myId))
    val myBody = grid.snakeTurnPoints.getOrElse(myId, Nil)

    //        newField = myField.map { f =>
    myGroupField = FieldByColumn(myId, myField.keys.groupBy(_.y).map { case (y, target) =>
      (y.toShort, Tool.findContinuous(target.map(_.x.toShort).toArray.sorted)) //read
    }.toList.groupBy(_._2).map { case (r, target) =>
      ScanByColumn(Tool.findContinuous(target.map(_._1).toArray.sorted), r)
    }.toList)


    //    println(s"=======myField:$myGroupField, myBody:$myBody")
  }

  private def transformData(data: Data4TotalSyncCondensed): Data4TotalSync = {
    val newBodyDt = data.bodyDetails.map {s=>
      val carnieId = grid.carnieMap(s.uid)
      BodyBaseInfo(carnieId, TurnInfo(s.turn.turnPoint, s.turn.pointOnField.map{p =>
        val carnieId = grid.carnieMap(p._2)
        (p._1, carnieId)
      }))
    }
    val newFieldDt = data.fieldDetails.map {f =>
      val id = grid.carnieMap(f.uid)
      FieldByColumn(id, f.scanField)
    }
    Data4TotalSync(data.frameCount, data.snakes, newBodyDt, newFieldDt)
  }

  override def render: Elem = {
    init()
    <div></div>
  }
}
