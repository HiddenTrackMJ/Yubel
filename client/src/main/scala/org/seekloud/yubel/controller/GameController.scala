package org.seekloud.yubel.controller

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.ActorRef
import org.seekloud.yubel.Boot
import org.seekloud.yubel.common.{Constant, Context}
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.yubel.paperClient._
import org.seekloud.yubel.scene.{GameScene, LayeredGameScene, PerformanceTool, SelectScene}
import javafx.animation.{Animation, AnimationTimer, KeyFrame, Timeline}
import javafx.scene.input.KeyCode
import javafx.util.Duration
import akka.actor.typed.scaladsl.adapter._
import org.slf4j.LoggerFactory
import org.seekloud.yubel.actor.PlayGameWebSocket
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import javafx.scene.media.{AudioClip, Media, MediaPlayer}
import java.awt.event.KeyEvent
//import org.seekloud.esheepapi.pb.observations.{ImgData, LayeredObservation}

/**
  * Created by dry on 2018/10/29.
  **/

class GameController(player: PlayerInfoInClient,
                     stageCtx: Context,
                     gameScene: GameScene,
//                     layeredGameScene: LayeredGameScene,
                     mode: Int =0,
                     frameRate: Int
                     ) {

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  private val playActor = Boot.system.spawn(PlayGameWebSocket.create(this), "playActor")

  var isGetKiller = false
  var killerInfo: scala.Option[String] = None
  var currentRank = List.empty[Sc]
  var rankInfo: scala.Option[Ranks] = None
  val bounds = Point(Boundary.w, Boundary.h)
  var grid = new GridOnClient(bounds)
  var firstCome = true
  val idGenerator = new AtomicInteger(1)
  var playBgm = true
//  var isSynced = false
  var isWin = false
  var isFirstTotalDataSync = false
  var exitFullScreen = false
  var winnerName = "unknown"
  var isContinues = true
  var btnFlag = true
  var winnerData : Option[Protocol.Data4TotalSync] = None
  val audioFinish = new AudioClip(getClass.getResource("/mp3/finish.mp3").toString)
  val audioKill = new AudioClip(getClass.getResource("/mp3/kill.mp3").toString)
  val audioWin = new AudioClip(getClass.getResource("/mp3/win.mp3").toString)
  val audioDie = new AudioClip(getClass.getResource("/mp3/killed.mp3").toString)
  val bgm4 = new AudioClip(getClass.getResource("/mp3/bgm4.mp3").toString)
  var pingMap = Map.empty[Short, Long] // id, 时间戳

  var pingId: Short = 0

  val bgmList = List(bgm4)
  var BGM = new AudioClip(getClass.getResource("/mp3/bgm4.mp3").toString)
  private val bgmAmount = bgmList.length
  var syncFrame: scala.Option[Protocol.SyncFrame] = None
  var syncGridData: scala.Option[Protocol.Data4TotalSync] = None
  var drawFunction: FrontProtocol.DrawFunction = FrontProtocol.DrawGameWait
  private var recallFrame: scala.Option[Int] = None
  private var stageWidth = stageCtx.getStage.getWidth.toInt
  private var stageHeight = stageCtx.getStage.getHeight.toInt
  private var isContinue = true
  private var myScore = BaseScore(0, 0, 0)
  private var maxArea: Int = 0
  private var winningData = WinData(0, Some(0), "")
  private val timeline = new Timeline()
  private var logicFrameTime = System.currentTimeMillis()
  private val animationTimer = new AnimationTimer() {
    override def handle(now: Long): Unit = {
      draw(System.currentTimeMillis() - logicFrameTime)
    }
  }

  def loseConnect(): Unit = {
    gameScene.drawGameOff(firstCome)
//    layeredGameScene.drawGameOff(firstCome)
  }

  def getRandom(s: Int):Int ={
    val rnd = new scala.util.Random
    rnd.nextInt(s)
  }

  def start(domain: String, mode: Int, img: Int): Unit = {
//    println(s"got accCode:${player.msg}")
    playActor ! PlayGameWebSocket.ConnectGame(player, domain, mode, img)
    addUserActionListen()
    startGameLoop()
  }

  def joinByRoomId(domain: String, roomId: Int, img: Int): Unit = {
    playActor ! PlayGameWebSocket.JoinByRoomId(player, domain, roomId, img)
    addUserActionListen()
    startGameLoop()
  }

  def createRoom(domain: String, mode: Int, img: Int, pwd: String): Unit = {
    playActor ! PlayGameWebSocket.CreateRoom(player, domain, mode, img, pwd)
    addUserActionListen()
    startGameLoop()
  }

  def startGameLoop(): Unit = { //渲染帧
    BGM = bgmList(getRandom(bgmAmount))
    BGM.play()
    logicFrameTime = System.currentTimeMillis()
    timeline.setCycleCount(Animation.INDEFINITE)
    val keyFrame = new KeyFrame(Duration.millis(frameRate), { _ =>
      logicLoop()
    })
    timeline.getKeyFrames.add(keyFrame)
    animationTimer.start()
    timeline.play()
  }

  private def logicLoop(): Unit = { //逻辑帧
    if(!stageCtx.getStage.isFullScreen && !exitFullScreen) {
      gameScene.resetScreen(Constant.CanvasWidth,Constant.CanvasHeight,Constant.CanvasWidth,Constant.CanvasHeight)
      if (grid.getGridData.snakes.exists(_.id == player.id) && !isWin && rankInfo.isDefined){
        gameScene.drawRank(player.id, grid.getGridData.snakes, rankInfo.get.currentRank, rankInfo.get.personalScore, rankInfo.get.personalRank, rankInfo.get.currentNum)
      }
      stageCtx.getStage.setWidth(Constant.CanvasWidth)
      stageCtx.getStage.setHeight(Constant.CanvasHeight)
      stageCtx.getStage.setX(200)
      stageCtx.getStage.setY(200)
      exitFullScreen = true
    }
    if(stageWidth != stageCtx.getStage.getWidth.toInt || stageHeight != stageCtx.getStage.getHeight.toInt){
      stageWidth = stageCtx.getStage.getWidth.toInt
      stageHeight = stageCtx.getStage.getHeight.toInt
      gameScene.resetScreen(stageWidth,stageHeight,stageWidth,stageHeight)
      if (grid.getGridData.snakes.exists(_.id == player.id) && !isWin && rankInfo.isDefined){
        gameScene.drawRank(player.id, grid.getGridData.snakes, rankInfo.get.currentRank, rankInfo.get.personalScore, rankInfo.get.personalRank, rankInfo.get.currentNum)
      }
      stageCtx.getStage.setWidth(stageWidth)
      stageCtx.getStage.setHeight(stageHeight)
      if (!isContinue) {
        if (isWin) {
          val winInfo = drawFunction.asInstanceOf[FrontProtocol.DrawGameWinBefore]
          gameScene.drawGameWin(player.id, winInfo.winnerName, winInfo.winData, winningData)
        } else {
          gameScene.drawGameDie(killerInfo, myScore, maxArea)
        }
      }
    }
    logicFrameTime = System.currentTimeMillis()
    if (pingId > 10000) pingId = 0  else pingId = (pingId + 1).toShort
    val curTime = System.currentTimeMillis()
    pingMap += (pingId -> curTime)
    playActor ! PlayGameWebSocket.MsgToService(Protocol.SendPingPacket(pingId))

    var isAlreadySendSync = false

    recallFrame match {
      case Some(-1) =>
        println("!!!!!!!!:NeedToSync")
        playActor ! PlayGameWebSocket.MsgToService(NeedToSync.asInstanceOf[UserAction])
        isAlreadySendSync = true
        recallFrame = None

      case Some(frame) =>
        val time1 = System.currentTimeMillis()
        println(s"before recall...frame:${grid.frameCount}")
        grid.historyDieSnake.filter { d => d._2.contains(player.id) && d._1 > frame }.keys.headOption match {
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
        grid.hsMap += grid.frameCount -> (grid.snakes, grid.grid, grid.snakeTurnPoints)
      }
      grid.initSyncGridData(syncGridData.get)
      addBackendInfo4Sync(grid.frameCount)
      syncGridData = None
    } else if (syncFrame.nonEmpty) { //局部数据仅同步帧号
      val frontend = grid.frameCount
      val backend = syncFrame.get.frameCount
      val advancedFrame = backend - frontend
      if (advancedFrame == 1) {
//        println(s"backend advanced frontend,frontend$frontend,backend:$backend")
        grid.updateOnClient()
        addBackendInfo(grid.frameCount)
      } else if (advancedFrame < 0 && grid.hsMap.get(backend).nonEmpty) {
        println(s"frontend advanced backend,frontend$frontend,backend:$backend")
        grid.setGridInGivenFrame(backend)
      } else if (advancedFrame == 0) {
        println(s"frontend equal to backend,frontend$frontend,backend:$backend")
      } else if (advancedFrame > 0 && advancedFrame < (grid.maxDelayed - 1)) {
        println(s"backend advanced frontend,frontend$frontend,backend:$backend")
        val endFrame = grid.historyDieSnake.filter { d => d._2.contains(player.id) && d._1 > frontend }.keys.headOption match {
          case Some(dieFrame) => Math.min(dieFrame - 1, backend)
          case None => backend
        }
        (frontend until endFrame).foreach { _ =>
          grid.updateOnClient()
          addBackendInfo(grid.frameCount)
        }
        println(s"after speed,frame:${grid.frameCount}")
      } else {
        if (!isAlreadySendSync) playActor ! PlayGameWebSocket.MsgToService(NeedToSync.asInstanceOf[UserAction])
      }
      syncFrame = None
    } else {
      grid.updateOnClient()
      addBackendInfo(grid.frameCount)
    }

    if (!isWin) {
      val gridData = grid.getGridData4Draw

      gridData.snakes.find(_.id == player.id) match {
        case Some(_) =>
          if(firstCome) firstCome = false
          //        if(playBgm) {
          //          BGM.play(30)
          //          playBgm = false
          //        }
          //        if(!BGM.isPlaying){
          //          BGM = bgmList(getRandom(bgmAmount))
          //          BGM.play(30)
          //        }
          drawFunction = FrontProtocol.DrawBaseGame(gridData)



        //      case None if isWin =>

        case None if isGetKiller && !firstCome =>
          if(btnFlag) {
            gameScene.group.getChildren.add(gameScene.backBtn)
            btnFlag = false
          }
          drawFunction = FrontProtocol.DrawGameDie(killerInfo)


        case _ =>
          drawFunction = FrontProtocol.DrawGameWait
      }
    }

  }

  def draw(offsetTime: Long): Unit = {
    drawFunction match {
      case FrontProtocol.DrawGameWait =>
        if(BGM.isPlaying){
          BGM.stop()
        }
        gameScene.drawGameWait()
//        layeredGameScene.drawGameWait()

      case FrontProtocol.DrawGameOff =>
        if(BGM.isPlaying){
          BGM.stop()
        }
        gameScene.drawGameOff(firstCome)
//        layeredGameScene.drawGameOff(firstCome)

      case FrontProtocol.DrawGameWinBefore(winner, winData) =>
        if(BGM.isPlaying){
          BGM.stop()
        }
        gameScene.drawGameWin(player.id, winner, winData,winningData)
//        layeredGameScene.drawGameWin(player.id, winner, winData,winningData)
        isContinue = false

      case FrontProtocol.DrawBaseGame(data) =>
        gameScene.draw(player.id, data, offsetTime, grid, currentRank.headOption.map(_.id).getOrElse(player.id))

        if (grid.killInfo.nonEmpty) {
          val killBaseInfo = grid.killInfo.get
          if (killBaseInfo._4 == player.id &&  grid.barrageDuration == 100) {
//            println("I killed someone")
            audioKill.play()
          }
          gameScene.drawBarrage(killBaseInfo._2, killBaseInfo._3)
          grid.barrageDuration -= 1
          if (grid.barrageDuration == 0) grid.killInfo = None
        }

      case FrontProtocol.DrawGameDie(killerName, data) =>
        if(BGM.isPlaying){
          BGM.stop()
        }
        if (isContinue) audioDie.play()

        gameScene.drawGameDie(killerName, myScore, maxArea)
//        layeredGameScene.drawGameDie(killerName, myScore, maxArea)
        grid.killInfo = None
        isContinue = false
    }
  }

  def gameMessageReceiver(msg: WsSourceProtocol.WsMsgSource): Unit = {
    msg match {
      case Protocol.Id(id) =>
        log.debug(s"i receive my id:$id")

      case r@Protocol.BoardAction(carnieId, keyCode, frame, actionId, _) =>
        if (grid.snakes.contains(grid.yubelMap.getOrElse(carnieId, ""))) {
          val id = grid.yubelMap(carnieId)
          if (id == player.id) { //收到自己的进行校验是否与预判一致，若不一致则回溯
            if (grid.myActionHistory.get(actionId).isEmpty) { //前端没有该项，则加入
              grid.addActionWithFrame(id, keyCode, frame)
              if (frame < grid.frameCount) {
                println(s"recall for my Action1,backend:$frame,frontend:${grid.frameCount}")
                recallFrame = grid.findRecallFrame(frame, recallFrame)
              }
            } else {
              if (grid.myActionHistory(actionId)._1 != keyCode || grid.myActionHistory(actionId)._2 != frame) { //若keyCode或则frame不一致则进行回溯
                grid.deleteActionWithFrame(id, grid.myActionHistory(actionId)._2)
                grid.addActionWithFrame(id, keyCode, frame)
                val miniFrame = Math.min(frame, grid.myActionHistory(actionId)._2)
                if (miniFrame < grid.frameCount) {
                  println(s"recall for my Action2,backend:$miniFrame,frontend:${grid.frameCount}")
                  recallFrame = grid.findRecallFrame(miniFrame, recallFrame)
                }
              }
              grid.myActionHistory -= actionId
            }
          }
        }

      case OtherAction(carnieId, keyCode, frame, _) =>
        Boot.addToPlatform{
          if (grid.snakes.contains(grid.yubelMap.getOrElse(carnieId, ""))) {
            val id = grid.yubelMap(carnieId)
            grid.addActionWithFrame(id, keyCode, frame)
            if (frame < grid.frameCount) {
              println(s"recall for other Action,backend:$frame,frontend:${grid.frameCount}")
              recallFrame = grid.findRecallFrame(frame, recallFrame)
            }
          }
        }

      case m@InitActions(actions) =>
        Boot.addToPlatform {
          println(s"recv $m")
          actions.foreach(gameMessageReceiver(_))
        }

//      case data: Protocol.NewFieldInfo =>
//        println(s"！！！当前帧号：${grid.frameCount}, 领地帧号：${data.frameCount}")
//        Boot.addToPlatform{
//          val fields = data.fieldDetails.map{f =>FieldByColumn(grid.carnieMap.getOrElse(f.uid, ""), f.scanField)}
////          if (fields.exists(_.uid == player.id)) audioFinish.play()
//          grid.historyFieldInfo += data.frameCount -> fields
//          if(data.frameCount == grid.frameCount){
//            addFieldInfo(data.frameCount)
//          } else if (data.frameCount < grid.frameCount) {
//            println(s"recall for NewFieldInfo,backend:${data.frameCount},frontend:${grid.frameCount}")
//            recallFrame = grid.findRecallFrame(data.frameCount - 1, recallFrame)
//          }
//        }
      case Protocol.NewData(frameCount, newSnakes, newField) =>
        Boot.addToPlatform{
          if (newSnakes.isDefined) {
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
            val fields = newField.get.map{f =>
              if(grid.yubelMap.get(f.uid).isEmpty) println(s"!!!!!!!error:::can not find id: ${f.uid} from yubelMap")
              FieldByColumn(grid.yubelMap.getOrElse(f.uid, ""), f.scanField)}
            if (fields.exists(_.uid == player.id)) {
//              println("myfield")
              audioFinish.play()
//              val myField = fields.filter(_.uid == player.id)
//              println(s"fieldDetail: $myField")
            }
            grid.historyFieldInfo += frameCount -> fields
            if(frameCount == grid.frameCount){
              addFieldInfo(frameCount)
            } else if (frameCount < grid.frameCount) {
              println(s"recall for NewFieldInfo,backend:$frameCount,frontend:${grid.frameCount}")
              recallFrame = grid.findRecallFrame(frameCount - 1, recallFrame)
            }
          }

          syncFrame = Some(SyncFrame(frameCount))
//          isSynced = true
        }


//      case Protocol.SomeOneWin(winner) =>
//        Boot.addToPlatform {
//          val finalData = grid.getWinData4Draw
//          drawFunction = FrontProtocol.DrawGameWin(winner, finalData)
////          winnerName = winner
////          winnerData = Some(finalData)
//          isWin = true
////          audioWin.play()
//          //        gameScene.drawGameWin(player.id, winner, finalData)
//          grid.cleanData()
//        }

//      case x@Protocol.WinnerBestScore(score) =>
//        log.debug(s"receive winnerBestScore msg:$x")
//          maxArea = Math.max(maxArea,score)

      case UserLeft(id) =>
        Boot.addToPlatform {
          println(s"user $id left:::")
          grid.yubelMap = grid.yubelMap.filterNot(_._2 == id)
          grid.cleanDiedSnakeInfo(List(id))
        }

      case x@Protocol.DeadPage(kill, area, playTime) =>
        println(s"recv userDead $x")
        Boot.addToPlatform {
          myScore = BaseScore(kill, area, playTime)
          maxArea = Math.max(maxArea, area)
        }


      case m@Protocol.Ranks(current, score, rank, currentNum) =>
        Boot.addToPlatform {
          currentRank = current
          rankInfo = Some(m)
//          maxArea = Math.max(maxArea, score.area)
//          if (grid.getGridData.snakes.exists(_.id == player.id) && !isWin && isSynced){
          if (grid.getGridData.snakes.exists(_.id == player.id) && !isWin){
            gameScene.drawRank(player.id, grid.getGridData.snakes, current, score, rank, currentNum)
//            layeredGameScene.drawRank(player.id, grid.getGridData.snakes, current)
//            layeredGameScene.drawHumanRank(player.id, grid.getGridData.snakes, currentRank)
          }
        }

      case data: Protocol.Data4TotalSync =>
        Boot.addToPlatform{
          if (!isFirstTotalDataSync) {
            grid.initSyncGridData(data) //立刻执行，不再等到逻辑帧
            isFirstTotalDataSync = true
          } else {
            syncGridData = Some(data)
          }
//          isSynced = true
        }

//      case data: Protocol.SyncFrame =>
//        syncFrame = Some(data)

//      case data: Protocol.NewSnakeInfo =>
//        Boot.addToPlatform{
//          data.snake.foreach { s => grid.carnieMap += s.carnieId -> s.id }
//          grid.historyNewSnake += data.frameCount -> (data.snake, data.filedDetails.map { f =>
//            FieldByColumn(grid.carnieMap.getOrElse(f.uid, ""), f.scanField)
//          })
//          if(data.frameCount == grid.frameCount){
//            addNewSnake(data.frameCount)
//          } else if (data.frameCount < grid.frameCount) {
//            println(s"recall for NewSnakeInfo,backend:${data.frameCount},frontend:${grid.frameCount}")
//            recallFrame = grid.findRecallFrame(data.frameCount - 1, recallFrame)
//          }
//        }

      case Protocol.UserDeadMsg(frame, deadInfo) =>
        Boot.addToPlatform{
          deadInfo.find{d => grid.yubelMap.getOrElse(d.carnieId, "") == player.id} match {
            case Some(myKillInfo) if myKillInfo.killerId.nonEmpty =>
              isGetKiller = true
              val info = grid.snakes.get(grid.yubelMap.getOrElse(myKillInfo.killerId.get, ""))
              killerInfo = info.map(_.name)

            case None =>

            case _ =>
              isGetKiller = true
              killerInfo = None
          }
          val deadList = deadInfo.map(baseInfo => grid.yubelMap.getOrElse(baseInfo.carnieId, ""))
//          println(s"==============deadList: $deadList")
          grid.historyDieSnake += frame -> deadList
          deadInfo.filter(_.killerId.nonEmpty).foreach { i =>
            val idOp = grid.yubelMap.get(i.carnieId)
            if (idOp.nonEmpty) {
              val id = idOp.get
              val name = grid.snakes.get(id).map(_.name).getOrElse("unknown")
              val killerName = grid.snakes.get(grid.yubelMap.getOrElse(i.killerId.get, "")).map(_.name).getOrElse("unknown")
              val killerId = grid.snakes.get(grid.yubelMap.getOrElse(i.killerId.get, "")).map(_.id).getOrElse("unknown")
              grid.killInfo = Some(id, name, killerName, killerId)
              grid.barrageDuration = 100
            }
          }
          if (frame < grid.frameCount) {
            println(s"recall for UserDeadMsg,backend:$frame,frontend:${grid.frameCount}")
            val deadRecallFrame = if (deadList.contains(player.id)) frame - 2 else frame - 1
            recallFrame = grid.findRecallFrame(deadRecallFrame, recallFrame)
          }
        }



      case x@Protocol.ReceivePingPacket(actionId) =>
        Boot.addToPlatform{
          if (pingMap.get(actionId).nonEmpty) {
            PerformanceTool.receivePingPackage(pingMap(actionId))
            pingMap -= actionId
          }
        }

      case x@Protocol.WinData(winnerScore, yourScore, winner) =>
        log.debug(s"receive winningData msg:$x")
        Boot.addToPlatform{
          winningData = x
          if (winner == player.id) maxArea = Math.max(maxArea, winnerScore)
          val finalData = grid.getWinData4Draw
          drawFunction = FrontProtocol.DrawGameWinBefore(winner, finalData)
          isWin = true
          grid.cleanData()
        }


      case unknown@_ =>
        log.debug(s"i receive an unknown msg:$unknown")
    }
  }

  def addUserActionListen():Unit = {
    gameScene.viewCanvas.requestFocus()

    gameScene.viewCanvas.setOnKeyPressed{ event =>
      val key = event.getCode
      if (Constant.watchKeys.contains(key)) {
        val delay = if(mode==2) 2 else 1
        val frame = grid.frameCount + delay
//        val actionId = idGenerator.getAndIncrement()
//        val keyCode = Constant.keyCode2Byte(key)
//        grid.addActionWithFrame(player.id, keyCode, frame)
        key match {
          case KeyCode.SPACE =>
            drawFunction match {
              case FrontProtocol.DrawBaseGame(_) =>

              case _ =>
                isGetKiller = false
                killerInfo = None
                val msg: Protocol.UserAction = PressSpace
                playActor ! PlayGameWebSocket.MsgToService(msg)
            }

          case _ =>
            drawFunction match {
              case FrontProtocol.DrawBaseGame(_) =>
                val newKeyCode = Constant.keyCode2Byte(
                  if (mode == 1)
                    key match {
                      case KeyCode.LEFT => KeyCode.RIGHT
                      case KeyCode.RIGHT => KeyCode.LEFT
                      case KeyCode.DOWN => KeyCode.UP
                      case KeyCode.UP => KeyCode.DOWN
                      case _ => KeyCode.SPACE
                    } else key)

                val actionInfo = grid.getUserMaxActionFrame(player.id, frame)
                if (actionInfo._1 < frame + maxContainableAction && actionInfo._2 != newKeyCode) {
                  val actionId = idGenerator.getAndIncrement()
                  grid.addActionWithFrame(player.id, newKeyCode, actionInfo._1)
                  grid.myActionHistory += actionId -> (newKeyCode, actionInfo._1)
                  val msg: Protocol.UserAction = Protocol.Key(newKeyCode, frame, actionId)
                  playActor ! PlayGameWebSocket.MsgToService(msg)
                }

              case _ =>
            }
        }
      }
//        if (key != KeyCode.SPACE) {
//          grid.myActionHistory += actionId -> (keyCode, frame)
//        } else {
//          //数据重置
//          drawFunction match {
//            case FrontProtocol.DrawBaseGame(_) =>
//            case _ =>
//              val msg: Protocol.UserAction = PressSpace
//              playActor ! PlayGameWebSocket.MsgToService(msg)
//              grid.cleanData()
//              drawFunction = FrontProtocol.DrawGameWait
//              audioWin.stop()
//              audioDie.stop()
//              firstCome = true
//              if(isWin){
//                isWin = false
//                winnerName = "unknown"
//              }
//              animationTimer.start()
//              isContinue = true
//              gameScene.group.getChildren.remove(gameScene.backBtn)
//              btnFlag = true
//          }
//        }
//        val newKeyCode =
//          if(mode == 1)
//            key match {
//              case KeyCode.LEFT => KeyCode.RIGHT
//              case KeyCode.RIGHT => KeyCode.LEFT
//              case KeyCode.DOWN => KeyCode.UP
//              case KeyCode.UP => KeyCode.DOWN
//              case _ => KeyCode.SPACE
//            }
//          else key
//        println(s"onkeydown：${Key(player.id, Constant.keyCode2Int(newKeyCode), frame, actionId)}")
//        playActor ! PlayGameWebSocket.MsgToService(Protocol.Key(player.id, Constant.keyCode2Int(newKeyCode), frame, actionId))
//      }
    }

    gameScene.backBtn.setOnAction(_ => switchToSelecting())
  }

  def switchToSelecting():Unit = {
    //fixme 假设用户一次玩游戏时间不超过两小时，否则需要刷新token
    Boot.addToPlatform{
      println("come back to selectScene.")
      stageCtx.getStage.setFullScreen(false)
      Boot.system.stop(playActor.toUntyped)
      Boot.addToPlatform{
        val selectScene = new SelectScene()
        new SelectController(player, selectScene, stageCtx).showScene
      }
    }
  }

  def spaceKey(): Unit = {
    grid.cleanSnakeTurnPoint(player.id)
    grid.actionMap = grid.actionMap.filterNot(_._2.contains(player.id))
    drawFunction = FrontProtocol.DrawGameWait
    firstCome = true
    if (isWin) isWin = false
    myScore = BaseScore(0, 0, 0)
    isContinue = true
//    isSynced = false
    isGetKiller = false
    killerInfo = None
    Boot.addToPlatform{
      gameScene.group.getChildren.remove(gameScene.backBtn)
      btnFlag = true
    }
//    animationTimer.start()
    //                  backBtn.style.display="none"
    //                  rankCanvas.addEventListener("",null)
  }

  def addBackendInfo(frame: Int): Unit = {
    addFieldInfo(frame)
    addDieSnake(frame)
    addNewSnake(frame)
  }

  def addFieldInfo(frame: Int): Unit = {
    grid.historyFieldInfo.get(frame).foreach { data =>
//      if (data.nonEmpty) println(s"addFieldInfo:$frame")
      grid.addNewFieldInfo(data)
    }
  }

  def addDieSnake(frame: Int): Unit = {
    grid.historyDieSnake.get(frame).foreach { deadSnake =>
      grid.cleanDiedSnakeInfo(deadSnake)
    }
  }

  def addNewSnake(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
      if (newSnakes._1.map(_.id).contains(player.id) && !firstCome && !isContinue) spaceKey()
      newSnakes._1.foreach { s => grid.cleanSnakeTurnPoint(s.id) } //清理死前拐点
      grid.snakes ++= newSnakes._1.map(s => s.id -> s).toMap
      grid.addNewFieldInfo(newSnakes._2)
    }
  }

  def addBackendInfo4Sync(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
      if (newSnakes._1.map(_.id).contains(player.id) && !firstCome && !isContinue) spaceKey()
    }
  }

//  def getAllImage  = {
//    val imageList = layeredGameScene.layered.getAllImageData
//    val humanObservation : _root_.scala.Option[ImgData] = imageList.find(_._1 == "6").map(_._2)
//    val layeredObservation : LayeredObservation = LayeredObservation(
//      imageList.find(_._1 == "0").map(_._2),
//      imageList.find(_._1 == "1").map(_._2),
//      imageList.find(_._1 == "2").map(_._2),
//      imageList.find(_._1 == "3").map(_._2),
//      imageList.find(_._1 == "4").map(_._2),
//      imageList.find(_._1 == "5").map(_._2)
//    )
//    (humanObservation,layeredObservation, grid.frameCount.toInt)
//  }
}
