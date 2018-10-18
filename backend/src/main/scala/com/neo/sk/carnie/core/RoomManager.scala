package com.neo.sk.carnie.core

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import akka.NotUsed
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.neo.sk.carnie.paperClient.PlayGround.{Join, Left}
import com.neo.sk.carnie.paperClient.Protocol
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import com.neo.sk.carnie.paperClient.WsSourceProtocol


import scala.concurrent.duration._
import scala.util.{Failure, Success}


/**
  * Created by dry on 2018/10/12.
  **/
object RoomManager {
  private val log = LoggerFactory.getLogger(this.getClass)
  private case object LeftRoomKey
//  private val roomInUse = mutable.HashMap[Long,mutable.HashSet[(Long,Boolean)]]()//roomId->Set((uid,False))uid-->等待复活
  private val roomMap = mutable.HashMap[Int, mutable.HashSet[(Long, String)]]() //roomId->Set((userId, name))
  private val limitNum = 8
//  private val userMap = mutable.HashMap[Long, (Long, String)]() //(userId, (roomId, name))


  trait Command
  trait UserAction extends Command
  case class UserActionOnServer(id: Long, action: Protocol.UserAction) extends Command
  case class Join(id: Long, name: String, subscriber: ActorRef[WsSourceProtocol.WsMsgSource]) extends Command
  case class Left(id: Long, name: String) extends Command
  case object CompleteMsgFront extends Command
  case class FailMsgFront(ex: Throwable) extends Command
  private final case object BehaviorChangeKey
  private case class TimeOut(msg:String) extends Command
  private case class ChildDead[U](roomId:Int, name:String,childRef:ActorRef[U]) extends Command
  case class LeftRoom(uid:Long,tankId:Int,name:String,userOpt: Option[Long]) extends Command
  case class UserLeft(id: Long) extends Command

  private case object UnKnowAction extends Command

//  case class Key(id: Long, keyCode: Int, frameCount: Long, actionId: Int) extends UserAction
//  case class TextInfo(msg: String) extends UserAction
//  case class SendPingPacket(id: Long, createTime: Long) extends UserAction
//  case class NeedToSync(id: Long) extends UserAction with Command


  def create():Behavior[Command] = {
    Behaviors.setup[Command]{ ctx =>
        implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
        Behaviors.withTimers[Command]{implicit timer =>
          val roomIdGenerator = new AtomicInteger(1)

          idle(roomIdGenerator)
        }
    }
  }

  def idle(roomIdGenerator:AtomicInteger)(implicit stashBuffer: StashBuffer[Command],timer:TimerScheduler[Command]) = {
    Behaviors.receive[Command]{(ctx,msg) =>
      msg match {
        case msg@Join(id, name, subscriber) =>
          log.info(s"got $msg")
          if(roomMap.isEmpty) {
            val roomId = roomIdGenerator.getAndIncrement()
            roomMap.put(roomId, mutable.HashSet((id, name)))
            getRoomActor(ctx, roomId) ! RoomActor.JoinRoom(id, name, subscriber)
          } else {
            if(roomMap.exists(_._2.size < limitNum)) {
              val roomId = roomMap.filter(_._2.size < limitNum).head._1
              roomMap.put(roomId, roomMap(roomId) += ((id, name)))
              getRoomActor(ctx, roomId) ! RoomActor.JoinRoom(id, name, subscriber)
            }
            else {
              val roomId = roomIdGenerator.getAndIncrement()
              roomMap.put(roomId, mutable.HashSet((id, name)))
              getRoomActor(ctx, roomId) ! RoomActor.JoinRoom(id, name, subscriber)
            }
          }
          Behaviors.same

        case msg@Left(id, name) =>
          log.info(s"got $msg")
          val roomId = roomMap.filter(r => r._2.exists(u => u._1 == id)).head._1
          roomMap.update(roomId, roomMap(roomId).-((id, name)))
          getRoomActor(ctx, roomId) ! RoomActor.LeftRoom(id, name)
          Behaviors.same

        case UserActionOnServer(id, action) =>
//          log.info(s"got $msg")
          if(roomMap.exists(r => r._2.exists(u => u._1 == id))) {
            val roomId = roomMap.filter(r => r._2.exists(u => u._1 == id)).head._1
            getRoomActor(ctx, roomId) ! RoomActor.UserActionOnServer(id, action)
          }
          Behaviors.same


        case ChildDead(roomId, child,childRef) =>
          log.debug(s"roomManager 不再监管room:$child,$childRef")
          ctx.unwatch(childRef)
          roomMap.remove(roomId)
          Behaviors.same

        case UserLeft(id) =>
          log.debug(s"got Terminated id = $id")
          val roomId = roomMap.filter(r => r._2.exists(u => u._1 == id)).head._1
          val filterUserInfo = roomMap(roomId).find(_._1 == id)
          if (filterUserInfo.nonEmpty){
            roomMap.update(roomId, roomMap(roomId).-(filterUserInfo.get))
          }
          Behaviors.same

        case unknow =>
          Behaviors.same
      }
    }
  }

  private def sink(actor: ActorRef[Command], id: Long, name: String) = ActorSink.actorRef[Command](
    ref = actor,
    onCompleteMessage = Left(id, name),
    onFailureMessage = FailMsgFront.apply
  )

  def joinGame(actor:ActorRef[RoomManager.Command], userId: Long, name: String): Flow[Protocol.UserAction, WsSourceProtocol.WsMsgSource, Any] = {
    val in = Flow[Protocol.UserAction]
      .map {
        case action@Protocol.Key(id, keyCode, frameCount, actionId) => UserActionOnServer(id, action)
        case action@Protocol.SendPingPacket(id, createTime) => UserActionOnServer(id, action)
        case action@Protocol.NeedToSync(id) => UserActionOnServer(id, action)
        case _ => UnKnowAction
      }
      .to(sink(actor, userId, name))

    val out =
      ActorSource.actorRef[WsSourceProtocol.WsMsgSource](
        completionMatcher = {
          case WsSourceProtocol.CompleteMsgServer ⇒
        },
        failureMatcher = {
          case WsSourceProtocol.FailMsgServer(e)  ⇒ e
        },
        bufferSize = 64,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue(outActor => actor ! Join(userId, name, outActor))

    Flow.fromSinkAndSource(in, out)
  }

  private def getRoomActor(ctx:ActorContext[Command],roomId:Int) = {
    val childName = s"room_$roomId"
    ctx.child(childName).getOrElse{
      val actor = ctx.spawn(RoomActor.create(roomId),childName)
      ctx.watchWith(actor,ChildDead(roomId, childName,actor))
      actor

    }.upcast[RoomActor.Command]
  }
}
