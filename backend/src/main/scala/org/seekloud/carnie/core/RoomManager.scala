package org.seekloud.carnie.core

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import org.seekloud.carnie.paperClient.Protocol
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import org.seekloud.carnie.core.RoomActor.UserDead
import org.seekloud.carnie.paperClient.Protocol.SendPingPacket
import org.seekloud.carnie.paperClient.WsSourceProtocol
import org.seekloud.carnie.ptcl.RoomApiProtocol.{CommonRsp, PlayerIdName, RecordFrameInfo}
import org.seekloud.carnie.common.AppSettings


/**
  * Created by dry on 2018/10/12.
  **/
object RoomManager {
  private val log = LoggerFactory.getLogger(this.getClass)

  private val roomMap = mutable.HashMap[Int, (Int, Option[String], mutable.HashSet[(String, String)])]() //roomId->(mode,pwd,Set((userId, name)))
  private val roomMap4Watcher = mutable.HashMap[Int, (Int, mutable.HashSet[String])]() //roomId->(mode,Set((userId)))
  private val limitNum = AppSettings.limitNum

  trait Command

  trait UserAction extends Command

  case class UserActionOnServer(id: String, action: Protocol.UserAction) extends Command

  case class CreateRoom(userId: String, name: String, mode: Int, img: Int, pwd: Option[String] = None, subscriber: ActorRef[WsSourceProtocol.WsMsgSource]) extends Command

  case class Join(id: String, name: String, mode: Int, img: Int, subscriber: ActorRef[WsSourceProtocol.WsMsgSource]) extends Command

  case class JoinByRoomId(id: String, roomId: Int, name: String, img: Int, subscriber: ActorRef[WsSourceProtocol.WsMsgSource]) extends Command

  case class Left(id: String, name: String) extends Command

  case class WatcherLeft(roomId: Int, playerId: String) extends Command

  case class StartReplay(recordId: Long, playedId: String, frame: Int, subscriber: ActorRef[WsSourceProtocol.WsMsgSource], playerId: String) extends Command

  case class StopReplay(recordId: Long, playerId: String) extends Command

  case class FindRoomId(pid: String, reply: ActorRef[Option[Int]]) extends Command

  case class FindPlayerList(roomId: Int, reply: ActorRef[List[PlayerIdName]]) extends Command

  case class ReturnRoomMap(reply: ActorRef[mutable.HashMap[Int, (Int, Option[String], mutable.HashSet[(String, String)])]]) extends Command

  case class VerifyPwd(roomId: Int, pwd: String, reply: ActorRef[Boolean]) extends Command

  case class FindAllRoom(reply: ActorRef[List[Int]]) extends Command

  case class FindAllRoom4Client(reply: ActorRef[List[String]]) extends Command

  case class JudgePlaying(userId: String, reply: ActorRef[Boolean]) extends Command

  case class JudgePlaying4Watch(roomId: Int, userId: String, reply: ActorRef[Boolean]) extends Command

  case class GetRecordFrame(recordId: Long, playerId: String, replyTo: ActorRef[CommonRsp]) extends Command

  case object CompleteMsgFront extends Command

  case class FailMsgFront(ex: Throwable) extends Command

  private case class TimeOut(msg: String) extends Command

  private case class ChildDead[U](roomId: Int, name: String, childRef: ActorRef[U]) extends Command

  case class BotsJoinRoom(roomId: Int, bots: List[(String, String)]) extends Command

  case class PreWatchGame(roomId: Int, playerId: String, userId: String, subscriber: ActorRef[WsSourceProtocol.WsMsgSource]) extends Command

  private case object UnKnowAction extends Command

  def create(): Behavior[Command] = {
    Behaviors.setup[Command] { ctx =>
      implicit val stashBuffer: StashBuffer[Command] = StashBuffer[Command](Int.MaxValue)
      Behaviors.withTimers[Command] { implicit timer =>
        val roomIdGenerator = new AtomicInteger(1000)
        idle(roomIdGenerator)
      }
    }
  }

  def idle(roomIdGenerator: AtomicInteger)(implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case m@CreateRoom(id, name, mode, img, pwd, subscriber) =>
          log.info(s"got $m")
          val roomId = roomIdGenerator.getAndIncrement()
          roomMap += roomId -> (mode , pwd, mutable.HashSet((id, name)))
          println(roomMap)
          getRoomActor(ctx, roomId, mode) ! RoomActor.JoinRoom(id, name, subscriber, img)
          subscriber ! Protocol.RoomId(roomId.toString)
          Behaviors.same

        case m@JoinByRoomId(id, roomId, name, img, subscriber) =>
          log.info(s"got: $m")
          if(roomMap.exists(_._1==roomId)){
            val mode = roomMap(roomId)._1
            roomMap += roomId -> (mode, roomMap(roomId)._2, roomMap(roomId)._3 + ((id, name)))
            getRoomActor(ctx, roomId, mode) ! RoomActor.JoinRoom(id, name, subscriber, img)
          } else
            log.info(s"房间不存在：$roomId")
          Behaviors.same

        case msg@Join(id, name, mode, img, subscriber) =>
          log.info(s"got $msg")
          if (roomMap.nonEmpty && roomMap.exists(r => r._2._1 == mode && r._2._3.size < limitNum && r._2._2.isEmpty)) {
            val rooms = roomMap.filter(r => r._2._1 == mode && r._2._3.size < limitNum && r._2._2.isEmpty).map(
              r => (r._1, r._2._3.size))
            val maxUsersNum = rooms.values.max
            val roomId = rooms.filter(_._2 == maxUsersNum).head._1
            roomMap.put(roomId, (mode, roomMap(roomId)._2, roomMap(roomId)._3 + ((id, name))))
            getRoomActor(ctx, roomId, mode) ! RoomActor.JoinRoom(id, name, subscriber, img)
          } else {
            val roomId = roomIdGenerator.getAndIncrement()
            roomMap.put(roomId, (mode, None, mutable.HashSet((id, name))))//默认无密码
            getRoomActor(ctx, roomId, mode) ! RoomActor.JoinRoom(id, name, subscriber, img)
          }
          Behaviors.same

        case UserDead(roomId, mode, users) =>
          try {
            getRoomActor(ctx, roomId, mode) ! RoomActor.UserDead(roomId, mode, users)
          } catch {
            case e: Exception =>
              log.error(s"user dead error:$e")
          }
          Behaviors.same

        case StartReplay(recordId, playedId, frame, subscriber, playerId) =>
          log.info(s"got $msg")
          getGameReplay(ctx, recordId, playerId) ! GameReplay.InitReplay(subscriber, playedId, frame)
          Behaviors.same

        case GetRecordFrame(recordId, playerId, replyTo) =>
          //          log.info(s"got $msg")
          getGameReplay(ctx, recordId, playerId) ! GameReplay.GetRecordFrame(playerId, replyTo)
          Behaviors.same

        case StopReplay(recordId, playerId) =>
          getGameReplay(ctx, recordId, playerId) ! GameReplay.StopReplay()
          Behaviors.same

        case JudgePlaying(userId, reply) =>
          val rst = roomMap.map(_._2._3.exists(_._1 == userId)).toList.contains(true)
          reply ! rst
          Behaviors.same

        case JudgePlaying4Watch(roomId, userId, reply) =>
          if(roomMap.contains(roomId)) {
            val msg = roomMap.filter(_._1==roomId).head._2._3.exists(_._1==userId)
            reply ! msg
            Behaviors.same
          } else {
            log.debug(s"got wrong roomId: $roomId")
            Behaviors.same
          }

        case m@PreWatchGame(roomId, playerId, userId, subscriber) =>
          log.info(s"got $m")
          val truePlayerId = if (playerId.contains("Set")) playerId.drop(4).dropRight(1) else playerId
          try {
            val mode = roomMap(roomId)._1
            val temp = roomMap4Watcher.getOrElse(roomId, (mode, mutable.HashSet.empty[String]))._2 + userId
//            println(s"temp: $temp")
//            roomMap4Watcher.put(roomId, (mode, temp))
            roomMap4Watcher += roomId -> (mode, temp)
//            println(s"room4watch: $roomMap4Watcher")
            getRoomActor(ctx, roomId, mode) ! RoomActor.WatchGame(truePlayerId, userId, subscriber)
          } catch {
            case e: Exception =>
              log.error(s"$msg got error: $e")
          }

          Behaviors.same

        case msg@Left(id, name) =>
          log.info(s"got $msg")
          try {
            val roomId = roomMap.filter(r => r._2._3.exists(u => u._1 == id)).head._1
            roomMap.update(roomId, (roomMap(roomId)._1, roomMap(roomId)._2, roomMap(roomId)._3 -((id, name))))
            val mode = roomMap(roomId)._1
            val humanPlayers = roomMap(roomId)._3.filter(!_._1.contains("bot"))
            if(humanPlayers.isEmpty) {
              val childName = s"room_$roomId-mode_$mode"
              val actor = getRoomActor(ctx, roomId, mode)
              ctx.self ! ChildDead(roomId, childName, actor)
            }
            getRoomActor(ctx, roomId, mode) ! RoomActor.LeftRoom(id, name)
          } catch {
            case e: Exception =>
              log.error(s"$msg got error: $e")
          }

          Behaviors.same

        case msg@WatcherLeft(roomId, userId) =>
          log.info(s"got $msg")
          try {
            val mode = roomMap(roomId)._1
            roomMap4Watcher.get(roomId) match {
              case Some(v) =>
                v._2 -= userId
                roomMap4Watcher += (roomId -> v)
              case _ =>
            }
//            println(s"watchLeft room4watch: $roomMap4Watcher")
            getRoomActor(ctx, roomId, mode) ! RoomActor.WatcherLeftRoom(userId)
          } catch {
            case e: Exception =>
              log.error(s"$msg got error: $e")
          }

          Behaviors.same

        case m@UserActionOnServer(id, action) =>
          if (roomMap.exists(r => r._2._3.exists(u => u._1 == id))) {
            val roomId = roomMap.filter(r => r._2._3.exists(u => u._1 == id)).head._1
            val mode = roomMap(roomId)._1
            getRoomActor(ctx, roomId, mode) ! RoomActor.UserActionOnServer(id, action)
          }
          if(roomMap4Watcher.exists(_._2._2.contains(id))) {
            val roomId = roomMap4Watcher.filter(_._2._2.contains(id)).head._1
            val mode = roomMap4Watcher(roomId)._1
            getRoomActor(ctx, roomId, mode) ! RoomActor.UserActionOnServer(id, action)
          }

          Behaviors.same


        case ChildDead(roomId, child, childRef) =>
          log.debug(s"roomManager 不再监管room:$child,$childRef")
          ctx.unwatch(childRef)
          roomMap.remove(roomId)
          Behaviors.same

        case FindRoomId(pid, reply) =>
          log.debug(s"got playerId = $pid")
          reply ! roomMap.find(r => r._2._3.exists(i => i._1 == pid)).map(_._1)
          Behaviors.same

        case m@VerifyPwd(roomId, pwd, reply) =>
          log.debug(s"got $m")
          val temp = roomMap.find(_._1 == roomId)
          val rst = if(temp.nonEmpty && temp.get._2._2.get == pwd) true else false
          reply ! rst
          Behaviors.same

        case FindPlayerList(roomId, reply) =>
          log.debug(s"${ctx.self.path} got roomId = $roomId")
          val roomInfo = roomMap.get(roomId)
          val replyMsg = if (roomInfo.nonEmpty) {
            roomInfo.get._3.toList.map { p => PlayerIdName(p._1, p._2) }
          } else Nil
          reply ! replyMsg
          Behaviors.same

        case ReturnRoomMap(reply) =>
          log.info(s"got room map")
          reply ! roomMap
          Behaviors.same

        case FindAllRoom(reply) =>
          log.info(s"got all room")
          reply ! roomMap.keySet.toList
          Behaviors.same

        case FindAllRoom4Client(reply) =>
          log.debug(s"got all room")
          log.info(s"roomIds: ${roomMap.keys}")
          reply ! roomMap.map{i => s"${i._1}-${i._2._1}-${i._2._2.nonEmpty}"}.toList //roomId-mode-pwd(t/f)
          Behaviors.same

        case BotsJoinRoom(roomId, bots) =>
          if (roomMap.get(roomId).nonEmpty) {
            val userInRoom = roomMap(roomId)._3
            roomMap += roomId -> roomMap(roomId).copy(_3 = userInRoom ++ bots)
          }
          Behaviors.same

        case unknown =>
          log.debug(s"${ctx.self.path} receive a msg unknown:$unknown")
          Behaviors.unhandled
      }
    }
  }

  //Left,StopRePlay,WatcherLeft合并
  private def sink(actor: ActorRef[Command], id: String, name: String) = ActorSink.actorRef[Command](
    ref = actor,
    onCompleteMessage = Left(id, name),
    onFailureMessage = FailMsgFront.apply
  )

  private def sink4Replay(actor: ActorRef[Command], recordId: Long, playerId: String) = ActorSink.actorRef[Command](
    ref = actor,
    onCompleteMessage = StopReplay(recordId, playerId),
    onFailureMessage = FailMsgFront.apply
  )

  private def sink4WatchGame(actor: ActorRef[Command], roomId: Int, userId: String) = ActorSink.actorRef[Command](
    ref = actor,
    onCompleteMessage = WatcherLeft(roomId, userId),
    onFailureMessage = FailMsgFront.apply
  )

  def joinGame(actor: ActorRef[RoomManager.Command], userId: String, name: String, mode: Int, img: Int): Flow[Protocol.UserAction, WsSourceProtocol.WsMsgSource, Any] = {
    val in = Flow[Protocol.UserAction]
      .map {
        case action@Protocol.Key(_, _, _) => UserActionOnServer(userId, action)
        case action@Protocol.Keys(_, _, _, _) => UserActionOnServer(userId, action)
        case action@Protocol.SendPingPacket(_) => UserActionOnServer(userId, action)
        case action@Protocol.NeedToSync => UserActionOnServer(userId, action)
        case action@Protocol.PressSpace => UserActionOnServer(userId, action)
        case _ => UnKnowAction
      }
      .to(sink(actor, userId, name))

    val out =
      ActorSource.actorRef[WsSourceProtocol.WsMsgSource](
        completionMatcher = {
          case WsSourceProtocol.CompleteMsgServer ⇒
        },
        failureMatcher = {
          case WsSourceProtocol.FailMsgServer(e) ⇒ e
        },
        bufferSize = 64,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue{outActor => actor ! Join(userId, name, mode, img, outActor)}

    Flow.fromSinkAndSource(in, out)
  }

  def joinGameByRoomId(actor: ActorRef[RoomManager.Command], userId: String, name: String, img: Int, roomId: Int): Flow[Protocol.UserAction, WsSourceProtocol.WsMsgSource, Any] = {
    val in = Flow[Protocol.UserAction]
      .map {
        case action@Protocol.Key(_, _, _) => UserActionOnServer(userId, action)
        case action@Protocol.Keys(_, _, _, _) => UserActionOnServer(userId, action)
        case action@Protocol.SendPingPacket(_) => UserActionOnServer(userId, action)
        case action@Protocol.NeedToSync => UserActionOnServer(userId, action)
        case action@Protocol.PressSpace => UserActionOnServer(userId, action)
        case _ => UnKnowAction
      }
      .to(sink(actor, userId, name))

    val out =
      ActorSource.actorRef[WsSourceProtocol.WsMsgSource](
        completionMatcher = {
          case WsSourceProtocol.CompleteMsgServer ⇒
        },
        failureMatcher = {
          case WsSourceProtocol.FailMsgServer(e) ⇒ e
        },
        bufferSize = 64,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue(outActor => actor ! JoinByRoomId(userId, roomId, name, img, outActor))

    Flow.fromSinkAndSource(in, out)
  }

  def createRoom(actor: ActorRef[RoomManager.Command], userId: String, name: String, mode: Int, img: Int, pwd: Option[String]): Flow[Protocol.UserAction, WsSourceProtocol.WsMsgSource, Any] = {
    val in = Flow[Protocol.UserAction]
      .map {
        case action@Protocol.Key(_, _, _) => UserActionOnServer(userId, action)
        case action@Protocol.Keys(_, _, _, _) => UserActionOnServer(userId, action)
        case action@Protocol.SendPingPacket(_) => UserActionOnServer(userId, action)
        case action@Protocol.NeedToSync => UserActionOnServer(userId, action)
        case action@Protocol.PressSpace => UserActionOnServer(userId, action)
        case _ => UnKnowAction
      }
      .to(sink(actor, userId, name))

    val out =
      ActorSource.actorRef[WsSourceProtocol.WsMsgSource](
        completionMatcher = {
          case WsSourceProtocol.CompleteMsgServer ⇒
        },
        failureMatcher = {
          case WsSourceProtocol.FailMsgServer(e) ⇒ e
        },
        bufferSize = 64,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue(outActor => actor ! CreateRoom(userId, name, mode, img, pwd, outActor))

    Flow.fromSinkAndSource(in, out)
  }

  def watchGame(actor: ActorRef[RoomManager.Command], roomId: Int, playerId: String, userId: String): Flow[Protocol.UserAction, WsSourceProtocol.WsMsgSource, Any] = {
    val in = Flow[Protocol.UserAction]
      .map {
        case action@Protocol.SendPingPacket(_) => UserActionOnServer(userId, action)
        case action@Protocol.NeedToSync => UserActionOnServer(userId, action)
        case _ => UnKnowAction
      }
      .to(sink4WatchGame(actor, roomId, userId))

    val out =
      ActorSource.actorRef[WsSourceProtocol.WsMsgSource](
        completionMatcher = {
          case WsSourceProtocol.CompleteMsgServer ⇒
        },
        failureMatcher = {
          case WsSourceProtocol.FailMsgServer(e) ⇒ e
        },
        bufferSize = 64,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue(outActor => actor ! PreWatchGame(roomId, playerId, userId, outActor))

    Flow.fromSinkAndSource(in, out)
  }

  def replayGame(actor: ActorRef[RoomManager.Command], recordId: Long, playedId: String, frame: Int, playerId: String): Flow[Protocol.UserAction, WsSourceProtocol.WsMsgSource, Any] = {
    val in = Flow[Protocol.UserAction]
      .map { _ => UnKnowAction}
      .to(sink4Replay(actor, recordId, playerId))

    val out =
      ActorSource.actorRef[WsSourceProtocol.WsMsgSource](
        completionMatcher = {
          case WsSourceProtocol.CompleteMsgServer ⇒
        },
        failureMatcher = {
          case WsSourceProtocol.FailMsgServer(e) ⇒ e
        },
        bufferSize = 64,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue(outActor => actor ! StartReplay(recordId, playedId, frame, outActor, playerId))

    Flow.fromSinkAndSource(in, out)
  }

  private def getRoomActor(ctx: ActorContext[Command], roomId: Int, mode: Int) = {
    val childName = s"room_$roomId-mode_$mode"
    ctx.child(childName).getOrElse {
      val actor = ctx.spawn(RoomActor.create(roomId, mode), childName)
//      ctx.watchWith(actor, ChildDead(roomId, childName, actor))
      actor

    }.upcast[RoomActor.Command]
  }

  private def getGameReplay(ctx: ActorContext[Command], recordId:Long, playerId: String) = {
    val childName = s"gameReplay--$recordId--$playerId"
    ctx.child(childName).getOrElse {
      val actor = ctx.spawn(GameReplay.create(recordId, playerId), childName)
      log.debug(s"new actor $childName!!!")
      actor
    }.upcast[GameReplay.Command]
  }
}
