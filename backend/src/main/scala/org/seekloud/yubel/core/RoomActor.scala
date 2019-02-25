package org.seekloud.yubel.core

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import org.seekloud.yubel.paperClient.Protocol._
import org.slf4j.LoggerFactory
import org.seekloud.yubel.paperClient._
import org.seekloud.yubel.Boot.roomManager
import org.seekloud.yubel.common.AppSettings
import scalikejdbc.TxBoundary.Future

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.language.postfixOps
import concurrent.duration._
//import org.seekloud.carnie.Boot.{executor, scheduler, timeout, tokenActor}
import akka.actor.typed.scaladsl.AskPattern._

/**
  * Created by dry on 2018/10/12.
  **/

object RoomActor {

  private val log = LoggerFactory.getLogger(this.getClass)
  val border = Point(BorderSize.w, BorderSize.h)


  private val waitingTime4CloseWs = 15.minutes
//  private val waitingTime4CloseWs = 5.seconds



  private final case object SyncKey

  private case object UserDeadTimerKey extends Command

  sealed trait Command

  case class UserActionOnServer(id: String, action: Protocol.UserAction) extends Command

  case class JoinRoom(id: String, name: String, subscriber: ActorRef[WsSourceProtocol.WsMsgSource], img: Int) extends Command

  case class JoinRoom4Bot(id: String, name: String, botActor: ActorRef[BotActor.Command], img: Int) extends Command

  case class LeftRoom(id: String, name: String) extends Command

  case class SnakeDead(roomId: Int, mode: Int, users: List[(String, Short, Short)]) extends Command with RoomManager.Command // (id, kill, area)

  case class UserDead(roomId: Int, mode: Int, users: List[(String, Long)]) extends Command with RoomManager.Command

  private case class ChildDead[U](name: String, childRef: ActorRef[U]) extends Command

  case class WatchGame(playerId: String, userId: String, subscriber: ActorRef[WsSourceProtocol.WsMsgSource]) extends Command

  case class WatcherLeftRoom(userId: String) extends Command

  case class CloseWs(userId: String) extends Command

  private case object Sync extends Command

  case class UserInfo(name: String, startTime: Long, joinFrame: Long, img: Int)

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  case class TimeOut(msg: String) extends Command

  def create(roomId: Int, mode: Int): Behavior[Command] = {
    log.debug(s"Room Actor-$roomId start...")
    Behaviors.setup[Command] { ctx =>
      implicit val carnieIdGenerator: AtomicInteger = new AtomicInteger(1)
      Behaviors.withTimers[Command] {
        implicit timer =>
          val grid = new GridOnServer(border)
          //            implicit val sendBuffer = new MiddleBufferInJvm(81920)
          val frameRate = mode match {
            case 2 => Protocol.frameRate2
            case _ => Protocol.frameRate1
          }
          log.info(s"frameRate: $frameRate")
          timer.startPeriodicTimer(SyncKey, Sync, frameRate millis)
          idle(roomId, mode, grid, tickCount = 0l)
      }
    }
  }

  def idle(roomId: Int,
           mode: Int,
           grid: GridOnServer,
           userMap: mutable.HashMap[String, UserInfo] = mutable.HashMap[String, UserInfo](),
           userDeadList: mutable.HashMap[String, (Long, Int)] = mutable.HashMap.empty[String, (Long, Int)],
           watcherMap: mutable.HashMap[String, (String, Long)] = mutable.HashMap[String, (String, Long)](), //(watcherId, (playerId, GroupId))
           subscribersMap: mutable.HashMap[String, ActorRef[WsSourceProtocol.WsMsgSource]] = mutable.HashMap[String, ActorRef[WsSourceProtocol.WsMsgSource]](),
           tickCount: Long,
           gameEvent: mutable.ArrayBuffer[(Long, GameEvent)] = mutable.ArrayBuffer[(Long, GameEvent)](),
           firstComeList: List[String] = List.empty[String],
           botMap: mutable.HashMap[String, ActorRef[BotActor.Command]] = mutable.HashMap[String, ActorRef[BotActor.Command]](),
           yubelMap: mutable.HashMap[String, Byte] = mutable.HashMap[String, Byte]() //(id -> carnie)
          )(implicit yubelIdGenerator: AtomicInteger, timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case m@JoinRoom(id, name, subscriber, img) =>
          log.info(s"got JoinRoom $m")
          yubelMap.put(id, grid.generateCarnieId(yubelIdGenerator, yubelMap.values))
          userMap.put(id, UserInfo(name, System.currentTimeMillis(), tickCount, img))
          subscribersMap.put(id, subscriber)
          log.debug(s"subscribersMap: $subscribersMap")
//          println("img: "+ img)
          if (img == 3) grid.genBricks(0)
          grid.addBoard(id, roomId, name, -1, yubelMap(id))
          dispatchTo(subscribersMap, id, Protocol.Id(id))
          log.info(s"userMap.size:${userMap.size}, minplayerNum:${AppSettings.minPlayerNum}, botMap.size:${botMap.size}")
          idle(roomId, mode, grid, userMap, userDeadList, watcherMap, subscribersMap, tickCount, gameEvent,  id :: firstComeList, botMap, yubelMap)

        case UserDead(_, _, users) =>
          val frame = grid.frameCount
          val endTime = System.currentTimeMillis()
          users.foreach { u =>
            val id = u._1
            timer.startSingleTimer(UserDeadTimerKey + id, CloseWs(id), waitingTime4CloseWs)
            if (userMap.get(id).nonEmpty) {
              val startTime = userMap(id).startTime
              dispatchTo(subscribersMap, id, Protocol.DeadBoard(frame,u._2, ((endTime - startTime) / 1000).toShort))
            }
            userDeadList += id -> (endTime,10)
          }
          Behaviors.same

        case LeftRoom(id, name) =>
          log.debug(s"LeftRoom:::$id")
          dispatch(subscribersMap, Protocol.UserLeft(id))
          yubelMap.-=(id)
          if (userDeadList.contains(id))  {
            userDeadList -= id
            timer.cancel(UserDeadTimerKey + id)
          }

          grid.cleanDiedBoardInfo(List(id))
          subscribersMap.get(id).foreach(r => ctx.unwatch(r))
          userMap.remove(id)
          subscribersMap.remove(id)
          Behaviors.same



        case CloseWs(id) =>
          log.info(s"close ws")
          dispatchTo(subscribersMap, id, Protocol.CloseWs)
          Behaviors.same

        case UserActionOnServer(id, action) =>
          action match {
            case a@Keys(keyCode, frameCount, actionId,typ) =>
              if (grid.boardMap.get(id).nonEmpty) {
                val realFrame = grid.checkActionFrame(id, frameCount)
                grid.addActionWithFrame(id, keyCode, realFrame, typ)
                dispatchTo(subscribersMap, id, Protocol.BoardAction(grid.boardMap(id).yubelId, keyCode, realFrame, actionId, typ)) //发送自己的action
                dispatch(subscribersMap.filterNot(_._1 == id),
                  Protocol.OtherAction(grid.boardMap(id).yubelId, keyCode, realFrame, typ)) //给其他人发送消息
              }


            case SendPingPacket(pingId) =>
              dispatchTo(subscribersMap, id, Protocol.ReceivePingPacket(pingId))

            case NeedToSync =>
//              val data = grid.getGridData
              val data = grid.getAllData
              dispatchTo(subscribersMap, id, data)


            case PressSpace =>
              if (userDeadList.contains(id)) {
                timer.cancel(UserDeadTimerKey + id)
                val info = userMap.getOrElse(id, UserInfo("", -1L, -1L, 0))
                grid.addBoard(id, roomId, info.name, userDeadList(id)._2,
                  yubelMap.get(id) match {
                    case Some(carnieId) => carnieId
                    case None =>
                      val newCarnieId = grid.generateCarnieId(yubelIdGenerator, yubelMap.values)
                      yubelMap.put(id, newCarnieId)
                      newCarnieId
                  })
                grid.winPlayer = grid.winPlayer :+ id
              }
            case x@_ => println("unknown msg " + x)
          }
          Behaviors.same

        case Sync =>
          val curTime = System.currentTimeMillis()
          val frame = grid.frameCount //即将执行改帧的数据
//          dispatch(subscribersMap,Protocol.SyncFrame(frame))
//          val shouldNewSnake = if (grid.waitingListState) true else false
          val shouldNewSnake = if (grid.waitingBoardState) true else false
          grid.updateInService(shouldNewSnake, roomId, mode) //frame帧的数据执行完毕
          val allData = grid.getAllData
          if (allData.bricks.isEmpty) {
            subscribersMap.foreach( s =>
              dispatchTo(subscribersMap,s._1,Protocol.GameWin(grid.scoreMap.values.toList)))
            grid.cleanData()
            userMap.foreach { u =>
              if (!userDeadList.contains(u._1)) {
                timer.startSingleTimer(UserDeadTimerKey + u._1, CloseWs(u._1), waitingTime4CloseWs)
                userDeadList += u._1 -> (curTime, 11)
              }
            }

            grid.genBricks(grid.randomBC(1 , 2))
          }
          if (grid.winPlayer.nonEmpty) {
            grid.winPlayer.foreach( p =>
            dispatchTo(subscribersMap, p, grid.getAllData))
            grid.winPlayer = List.empty[String]
          }
//          println("bricks: " + allData.bricks.size)
          dispatch(subscribersMap.filter(s => firstComeList.contains(s._1)), allData)
          val endTime = System.currentTimeMillis()
          val deadList = grid.historyDieBoard.get(frame)
          if (deadList.nonEmpty) {
            val deadSnakesInfo = deadList.get.map { id =>
              if (grid.scoreMap.exists(_._1 == id)) {
                val info = grid.scoreMap.filter(_._1 == id).head
                (id, info._2.score)
              } else (id, -1L)
            }
            deadSnakesInfo.map{ u =>
              val id = u._1
              grid.boardMap -= id
              grid.ballMap -= id
              grid.scoreMap -= id
              timer.startSingleTimer(UserDeadTimerKey + id, CloseWs(id), waitingTime4CloseWs)
              if (userMap.get(id).nonEmpty) {
                val startTime = userMap(id).startTime
                dispatchTo(subscribersMap, id, Protocol.DeadBoard(frame,u._2, ((endTime - startTime) / 1000).toShort))
              }
              userDeadList += id -> (endTime, 10)
            }
            dispatch(subscribersMap.filterNot{ s =>
              deadList.get.contains(s._1)}, Protocol.OthersDead(frame,deadList.get))
          }

          val newBoards = grid.newBoardInfo
          if (newBoards.isDefined) {
            val newBoardList = newBoards.get._1.keys.toList
            val newAllData = newBoards.get
            newBoards.get._1.foreach{ nb =>
                dispatchTo(subscribersMap, nb._1, allData)
            }
            dispatch(subscribersMap.filterNot(s =>
              newBoardList.contains(s._1)), Protocol.NewBoard
            (DataExceptBrick(frame,newAllData._1,newAllData._2,newAllData._3)))
          }
          grid.newBoardInfo = None

          val othersVary = grid.othersVary
          if (othersVary.nonEmpty) {
            othersVary.foreach{ o =>
              dispatch(subscribersMap.filterNot(_._1 == o._1), Protocol.OthersVary(othersVary.filterNot(_._1 == o._1)))
            }
            grid.othersVary = Map.empty[String, Board]
          }

          val init = grid.boardActionMap.map{ m =>
            m._2.toList.map{ a =>
              Protocol.OtherAction(yubelMap.getOrElse(a._1,0), a._2._1.toByte, m._1, a._2._2)
            }
          }.toList.flatten
          dispatch(subscribersMap.filter(s => firstComeList.contains(s._1)), InitActions(init))

          //错峰发送
          for ((u, i) <- userMap) {
            if ((tickCount - i.joinFrame) % 20 == 5 && grid.scoreMap.exists(_._1 == u)) {
              dispatchTo(subscribersMap, u, Protocol.ScoreData(grid.scoreMap))
            }
          }

          for ((u, i) <- userMap) {
            if ((tickCount - i.joinFrame) % 10 == 1 && grid.boardMap.exists(_._1 == u)) {
              dispatchTo(subscribersMap, u, Protocol.SyncFrame(grid.frameCount))
            }
          }


          idle(roomId, mode, grid, userMap, userDeadList, watcherMap, subscribersMap, tickCount + 1, gameEvent , botMap = botMap, yubelMap = yubelMap)

        case ChildDead(child, childRef) =>
          log.debug(s"roomActor 不再监管 gameRecorder:$child,$childRef")
          ctx.unwatch(childRef)
          Behaviors.same

        case _ =>
          log.warn(s"${ctx.self.path} recv a unknow msg=$msg")
          Behaviors.unhandled
      }
    }

  }

  def dispatchTo(subscribers: mutable.HashMap[String, ActorRef[WsSourceProtocol.WsMsgSource]], id: String, gameOutPut: Protocol.GameMessage): Unit = {
    subscribers.get(id).foreach {
      _ ! gameOutPut
    }
  }

  def dispatch(subscribers: mutable.HashMap[String, ActorRef[WsSourceProtocol.WsMsgSource]], gameOutPut: Protocol.GameMessage): Unit = {
    subscribers.values.foreach {
      _ ! gameOutPut
    }
  }

  def dispatchToPlayerAndWatcher(subscribers: mutable.HashMap[String, ActorRef[WsSourceProtocol.WsMsgSource]],
                                 watcherMap: mutable.HashMap[String, (String, Long)],
                                 id: String, gameOutPut: Protocol.GameMessage): Unit = {
    (id :: watcherMap.filter(_._2._1 == id).keys.toList).foreach (u => dispatchTo(subscribers, u, gameOutPut))

  }


}
