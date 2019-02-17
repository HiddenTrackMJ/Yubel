package org.seekloud.carnie.controller

import java.awt.event.KeyEvent

import org.seekloud.carnie.Boot
import org.seekloud.carnie.actor.BotActor
import org.seekloud.carnie.paperClient._
import org.seekloud.carnie.paperClient.Protocol._
import javafx.animation.{Animation, AnimationTimer, KeyFrame, Timeline}
import javafx.util.Duration
import org.slf4j.LoggerFactory
import akka.actor.typed.scaladsl.adapter._
import org.seekloud.carnie.actor.BotActor.{Dead, Observation}
import org.seekloud.carnie.common.Context
import org.seekloud.carnie.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.carnie.scene.{GameScene, LayeredGameScene}
import org.seekloud.esheepapi.pb.observations.{ImgData, LayeredObservation}

/**
  * Created by dry on 2018/12/4.
  **/
class BotController(player: PlayerInfoInClient,
                    stageCtx: Context,
                    layeredGameScene: LayeredGameScene,
                    domain: String,
                    mode: Int =0,
                    ) {

  private val botActor = Boot.system.spawn(BotActor.create(this, player, domain), "botActor")

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  private var drawFunction: FrontProtocol.DrawFunction = FrontProtocol.DrawGameWait
  private var recallFrame: scala.Option[Int] = None

//  var allImageData:List[Array[Int]] = List.empty
  var currentRank = List.empty[Score]
  private val frameRate = 150
  var grid = new GridOnClient(Point(Boundary.w, Boundary.h))
  private val timeline = new Timeline()
  var syncGridData: scala.Option[Protocol.Data4TotalSync] = None
  var myCurrentRank = Score(player.id, player.name, 0)
  private var myActions = Map.empty[Int,Int]
  var rankInfo: scala.Option[Ranks] = None
  private var logicFrameTime = System.currentTimeMillis()
  var syncFrame: scala.Option[Protocol.SyncFrame] = None
  var isDead = false
  var isFirstTotalDataSync = false

  def startGameLoop(): Unit = { //渲染帧
    layeredGameScene.cleanGameWait(stageCtx.getStage.getWidth.toInt, stageCtx.getStage.getHeight.toInt)
    logicFrameTime = System.currentTimeMillis()
    timeline.setCycleCount(Animation.INDEFINITE)
    val keyFrame = new KeyFrame(Duration.millis(frameRate), { _ =>
      logicLoop()
    })
    timeline.getKeyFrames.add(keyFrame)
    timeline.play()
  }

  private def logicLoop(): Unit = { //逻辑帧
//    allImageData = getAllImage
    logicFrameTime = System.currentTimeMillis()

    var isAlreadySendSync = false

    recallFrame match {
      case Some(-1) =>
        println("!!!!!!!!:NeedToSync")
        botActor ! BotActor.MsgToService(NeedToSync.asInstanceOf[UserAction])
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

    if (!isDead) {
      val imageList = layeredGameScene.layered.getAllImageData
      val humanObservation: _root_.scala.Option[ImgData] = imageList.find(_._1 == "7").map(_._2)
      val layeredObservation: LayeredObservation = LayeredObservation(
        imageList.find(_._1 == "0").map(_._2),
        imageList.find(_._1 == "1").map(_._2),
        imageList.find(_._1 == "2").map(_._2),
        imageList.find(_._1 == "3").map(_._2),
        imageList.find(_._1 == "4").map(_._2),
        imageList.find(_._1 == "5").map(_._2),
        imageList.find(_._1 == "6").map(_._2),
        None
      )
      botActor ! Observation(humanObservation, Some(layeredObservation), grid.frameCount, true)
    } else {
      botActor ! Observation(None, None, grid.frameCount, false)
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
//        println(s"backend advanced frontend,frontend$frontend,backend:$backend")
        grid.updateOnClient()
        addBackendInfo(grid.frameCount)
      } else if (advancedFrame < 0 && grid.historyStateMap.get(backend).nonEmpty) {
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
        if (!isAlreadySendSync) botActor ! BotActor.MsgToService(NeedToSync.asInstanceOf[UserAction])
      }
      syncFrame = None
    } else {
      grid.updateOnClient()
      addBackendInfo(grid.frameCount)
    }

    val gridData = grid.getGridData4Draw
    gridData.snakes.find(_.id == player.id) match {
      case Some(_) =>
//        isDead = false
        val offsetTime = System.currentTimeMillis() - logicFrameTime
        if(rankInfo.isDefined)
        layeredGameScene.draw(currentRank, player.id, gridData, offsetTime, grid,
          currentRank.headOption.map(_.id).getOrElse(player.id),myActions,
          rankInfo.get.personalScore, rankInfo.get.personalRank, rankInfo.get.currentNum)
//        else {
//          layeredGameScene.drawGameWait(stageCtx.getStage.getWidth.toInt, stageCtx.getStage.getHeight.toInt)
//          layeredGameScene.cleanGameWait(stageCtx.getStage.getWidth.toInt, stageCtx.getStage.getHeight.toInt)
//        }
        drawFunction = FrontProtocol.DrawBaseGame(gridData)

//      case None =>
//        drawFunction = FrontProtocol.DrawGameDie(grid.getKiller(player.id).map(_._2))

      case _ =>
//        isDead = true
        drawFunction = FrontProtocol.DrawGameWait
    }
  }

  def gameMessageReceiver(msg: WsSourceProtocol.WsMsgSource): Unit = {
    msg match {
      case Protocol.Id(id) =>
        log.debug(s"i receive my id:$id")

      case Protocol.RoomId(roomId) =>
        botActor ! BotActor.RoomId(roomId)
        log.debug(s"i receive roomId:$roomId")

      case Protocol.BoardAction(carnieId, keyCode, frame, actionId, _) =>
        Boot.addToPlatform {
          if (grid.snakes.contains(grid.carnieMap.getOrElse(carnieId, ""))) {
            val id = grid.carnieMap(carnieId)
            if (id == player.id) { //收到自己的进行校验是否与预判一致，若不一致则回溯
              myActions += frame -> keyCode
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
        }

      case OtherAction(carnieId, keyCode, frame, _) =>
        Boot.addToPlatform {
          if (grid.snakes.contains(grid.carnieMap.getOrElse(carnieId, ""))) {
            val id = grid.carnieMap(carnieId)
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


//      case data: Protocol.SyncFrame =>
//        syncFrame = Some(data)

      case Protocol.SomeOneWin(winner) =>
        Boot.addToPlatform {
          val finalData = grid.getWinData4Draw
          drawFunction = FrontProtocol.DrawGameWin(winner, finalData)
          grid.cleanData()
        }

      case Protocol.UserDeadMsg(frame, deadInfo) =>
        Boot.addToPlatform{
          val deadList = deadInfo.map(baseInfo => grid.carnieMap.getOrElse(baseInfo.carnieId, ""))
          grid.historyDieSnake += frame -> deadList
          deadInfo.filter(_.killerId.nonEmpty).foreach { i =>
            val idOp = grid.carnieMap.get(i.carnieId)
            if (idOp.nonEmpty) {
              val id = idOp.get
              val name = grid.snakes.get(id).map(_.name).getOrElse("unknown")
              val killerName = grid.snakes.get(grid.carnieMap.getOrElse(i.killerId.get, "")).map(_.name).getOrElse("unknown")
              val killerId = grid.snakes.get(grid.carnieMap.getOrElse(i.killerId.get, "")).map(_.id).getOrElse("unknown")
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

      case Protocol.WinData(_, _, _) =>
        Boot.addToPlatform{
          grid.cleanData()
        }


//      case Protocol.UserLeft(id) =>
//        Boot.addToPlatform {
//          println(s"user $id left:::")
//          if (grid.snakes.contains(id)) grid.snakes -= id
//          grid.returnBackField(id)
//          grid.grid ++= grid.grid.filter(_._2 match { case Body(_, fid) if fid.nonEmpty && fid.get == id => true case _ => false }).map { g =>
//            Point(g._1.x, g._1.y) -> Body(g._2.asInstanceOf[Body].id, None)
//          }
//        }

      case UserLeft(id) =>
        Boot.addToPlatform {
          println(s"user $id left:::")
          grid.carnieMap = grid.carnieMap.filterNot(_._2 == id)
          grid.cleanDiedSnakeInfo(List(id))
        }


      case x@Protocol.DeadPage(_, _, _) =>
        println(s"recv userDead $x")
        Boot.addToPlatform {
//          isDead = true
//          botActor ! Dead
        }


      case x@Protocol.Ranks(current, scoreOp, _, _) =>
        Boot.addToPlatform {
//          println(s"rank!!! $score")
          currentRank = current
          rankInfo = Some(x)
          myCurrentRank = if(scoreOp.isEmpty) current.filter(_.id == player.id).head else scoreOp.get
          layeredGameScene.layered.drawRank(player.id,grid.getGridData.snakes,List(myCurrentRank))
        }

      case data: Protocol.Data4TotalSync =>
//        println("data!!!")
        Boot.addToPlatform{
          if (!isFirstTotalDataSync) {
            grid.initSyncGridData(data) //立刻执行，不再等到逻辑帧
            isFirstTotalDataSync = true
          } else {
            syncGridData = Some(data)
          }
        }

      case Protocol.NewData(frameCount, newSnakes, newField) =>
        Boot.addToPlatform{
          if (newSnakes.isDefined) {
            val data = newSnakes.get
            data.snake.foreach { s => grid.carnieMap += s.carnieId -> s.id }
            grid.historyNewSnake += frameCount -> (data.snake, data.filedDetails.map { f =>
              FieldByColumn(grid.carnieMap.getOrElse(f.uid, ""), f.scanField)
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
              if(grid.carnieMap.get(f.uid).isEmpty) println(s"!!!!!!!error:::can not find id: ${f.uid} from carnieMap")
              FieldByColumn(grid.carnieMap.getOrElse(f.uid, ""), f.scanField)}
            if (fields.exists(_.uid == player.id)) {
              val myField = fields.filter(_.uid == player.id)
              println(s"fieldDetail: $myField")
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

//      case data: Protocol.NewFieldInfo =>
////        println(s"====================new field")
//        Boot.addToPlatform {
//          val fields = data.fieldDetails.map{f =>FieldByColumn(grid.carnieMap.getOrElse(f.uid, ""), f.scanField)}
//          grid.historyFieldInfo += data.frameCount -> fields
//          if(data.frameCount == grid.frameCount){
//            addFieldInfo(data.frameCount)
//          } else if (data.frameCount < grid.frameCount) {
//            println(s"recall for NewFieldInfo,backend:${data.frameCount},frontend:${grid.frameCount}")
//            recallFrame = grid.findRecallFrame(data.frameCount - 1, recallFrame)
//          }
//        }

      case x@Protocol.ReceivePingPacket(_) =>
        Boot.addToPlatform{
        }

      case unknown@_ =>
        log.debug(s"i receive an unknown msg:$unknown")
    }
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
      botActor ! Dead
      isDead = true
      grid.cleanDiedSnakeInfo(deadSnake)
    }
  }

  def addNewSnake(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
//      if (newSnakes._1.map(_.id).contains(player.id) && !firstCome && !isContinue) spaceKey()
      newSnakes._1.foreach { s => grid.cleanSnakeTurnPoint(s.id) } //清理死前拐点
      grid.snakes ++= newSnakes._1.map(s => s.id -> s).toMap
      if (newSnakes._1.exists(_.id == player.id)) isDead = false
      grid.addNewFieldInfo(newSnakes._2)
    }
  }

  def addBackendInfo4Sync(frame: Int): Unit = {
    grid.historyNewSnake.get(frame).foreach { newSnakes =>
//      if (newSnakes._1.map(_.id).contains(player.id) && !firstCome && !isContinue) spaceKey()
    }
  }

//  def getAllImage  = {
//    Boot.addToPlatform {
//      val imageList = layeredGameScene.layered.getAllImageData
//      val humanObservation: _root_.scala.Option[ImgData] = imageList.find(_._1 == "7").map(_._2)
//      val layeredObservation: LayeredObservation = LayeredObservation(
//        imageList.find(_._1 == "0").map(_._2),
//        imageList.find(_._1 == "1").map(_._2),
//        imageList.find(_._1 == "2").map(_._2),
//        imageList.find(_._1 == "3").map(_._2),
//        imageList.find(_._1 == "4").map(_._2),
//        imageList.find(_._1 == "5").map(_._2),
//        imageList.find(_._1 == "6").map(_._2),
//        None
//      )
//      val observation = (humanObservation, Some(layeredObservation), grid.frameCount, true)
//      botActor ! Observation(observation)
//    }
//  }
}
