package org.seekloud.yubel.actor

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.scaladsl.{Keep, Sink}
import org.seekloud.yubel.bot.BotServer
import org.slf4j.LoggerFactory
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage, WebSocketRequest}
import akka.stream.OverflowStrategy
import akka.stream.typed.scaladsl.ActorSource
import akka.util.{ByteString, ByteStringBuilder}
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.byteobject.ByteObject.{bytesDecode, _}
import org.seekloud.byteobject.MiddleBufferInJvm

import scala.concurrent.Future
import org.seekloud.yubel.Boot.{executor, materializer, scheduler, system, timeout}
import org.seekloud.yubel.common.Constant
import org.seekloud.yubel.controller.BotController
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.yubel.paperClient.{Protocol, Score}
import org.seekloud.yubel.paperClient.WebSocketProtocol.PlayGamePara
import org.seekloud.esheepapi.pb.actions.Move
import org.seekloud.esheepapi.pb.api.{SimpleRsp, State}
import org.seekloud.esheepapi.pb.observations.{ImgData, LayeredObservation}

import scala.concurrent.duration.FiniteDuration
import scala.language.postfixOps
import concurrent.duration._

/**
  * Created by dry on 2018/12/3.
  **/

object BotActor {

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  sealed trait Command

  val idGenerator = new AtomicInteger(1)

  val delay = 2

  var observation: (Option[ImgData], Option[LayeredObservation], Int, Boolean) = (None, None, -1, false)

  private final case object BehaviorChangeKey

  case object Work extends Command

  case class CreateRoom(apiToken: String, password: String, replyTo: ActorRef[String]) extends Command

  case class RoomId(roomId: String) extends Command

  case class JoinRoom(roomId: String, apiToken: String, replyTo: ActorRef[SimpleRsp]) extends Command

  case object LeaveRoom extends Command

  case class Reincarnation(replyTo: ActorRef[SimpleRsp]) extends Command

  case object Dead extends Command

  case class Action(move: Move, replyTo: ActorRef[Long]) extends Command

  case class ReturnObservation(replyTo: ActorRef[(Option[ImgData], Option[LayeredObservation], Int, Boolean)]) extends Command

  case class ReturnObservationWithInfo(replyTo: ActorRef[(Option[ImgData], Option[LayeredObservation], Score, Int, Boolean)]) extends Command

  case class Observation(obs: (Option[ImgData], Option[LayeredObservation], Int, Boolean)) extends Command

  case class ReturnInform(replyTo: ActorRef[(Score, Long)]) extends Command

  case class MsgToService(sendMsg: WsSendMsg) extends Command

  case class TimeOut(msg: String) extends Command

  case class GetFrame(replyTo: ActorRef[Int]) extends Command

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command


  def create(botController: BotController, playerInfo: PlayerInfoInClient, domain: String): Behavior[Command] = {
    Behaviors.setup[Command] { ctx =>
      implicit val stashBuffer: StashBuffer[Command] = StashBuffer[Command](Int.MaxValue)
      Behaviors.withTimers { implicit timer =>
        ctx.self ! Work
        waitingForWork(botController, playerInfo, domain)
      }
    }
  }

  def waitingForWork(botController: BotController,
                     playerInfo: PlayerInfoInClient,
                     domain: String)(implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case Work =>
          val port = 5322//todo config

          val server = BotServer.build(port, executor, ctx.self, playerInfo.name, botController)
          server.start()
          log.debug(s"Server started at $port")
          sys.addShutdownHook {
            log.debug("JVM SHUT DOWN.")
            server.shutdown()
            log.debug("SHUT DOWN.")
          }
          log.debug("DONE.")
          waitingGame(botController, playerInfo, domain)

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown")
          Behaviors.unhandled
      }
    }
  }

  def waitingGame(botController: BotController,
                  playerInfo: PlayerInfoInClient,
                  domain: String
                 )(implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case CreateRoom(apiToken, pwd, replyTo) =>
          log.debug(s"recv $msg")
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(
            getCreateRoomWebSocketUri(domain, playerInfo.id, playerInfo.name, apiToken, pwd)))
          val source = getSource
          val sink = getSink(botController)
          val ((stream, response), closed) =
            source
              .viaMat(webSocketFlow)(Keep.both) // keep the materialized Future[WebSocketUpgradeResponse]
              .toMat(sink)(Keep.both) // also keep the Future[Done]
              .run()

          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
              ctx.self ! SwitchBehavior("waitingForRoomId", waitingForRoomId(stream, botController, playerInfo, replyTo))
              log.debug(s"switch behavior")
              botController.startGameLoop()
              Future.successful("connect success")
            } else {
              replyTo ! "error"
              ctx.self ! SwitchBehavior("waitingGame", waitingGame(botController, playerInfo, domain))
              throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
            }
          } //ws建立

          closed.onComplete { _ =>
            log.info("connect to service closed!")
            //
          } //ws断开
          connected.onComplete(i => log.info(i.toString))
          switchBehavior(ctx, "busy", busy())

        case JoinRoom(roomId, apiToken, replyTo) =>
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(
            getJoinRoomWebSocketUri(domain, roomId, playerInfo.id, playerInfo.name, apiToken)))
          val source = getSource
          val sink = getSink(botController)
          val ((stream, response), closed) =
            source
              .viaMat(webSocketFlow)(Keep.both) // keep the materialized Future[WebSocketUpgradeResponse]
              .toMat(sink)(Keep.both) // also keep the Future[Done]
              .run()

          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
              replyTo ! SimpleRsp(state = State.unknown, msg = "ok")
              botController.startGameLoop()
              Future.successful("connect success")
            } else {
              replyTo ! SimpleRsp(errCode = 10006, state = State.unknown, msg = "join room error")
              throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
            }
          } //ws建立

          closed.onComplete { _ =>
            log.info("connect to service closed!")
          } //ws断开
          connected.onComplete(i => log.info(i.toString))
          gaming(stream, botController, playerInfo)

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown")
          Behaviors.unhandled
      }
    }
  }

  def gaming(actor: ActorRef[Protocol.WsSendMsg],
             botController: BotController,
             playerInfo: PlayerInfoInClient)(implicit stashBuffer: StashBuffer[Command],
                                             timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case Action(move, replyTo) =>
          val actionNum = Constant.moveToKeyCode(move)
          if(actionNum != -1) {
            val actionId = idGenerator.getAndIncrement()
            val frame = botController.grid.frameCount
            actor ! Key(actionNum, frame, actionId)
            botController.grid.addActionWithFrame(playerInfo.id, actionNum, frame)
            replyTo ! frame
          } else replyTo ! -1L
          Behaviors.same

        case ReturnObservation(replyTo) =>
          replyTo ! observation
          Behaviors.same

        case ReturnObservationWithInfo(replyTo) =>
          replyTo ! (observation._1, observation._2, botController.myCurrentRank, observation._3, observation._4)
          Behaviors.same

        case ReturnInform(replyTo) =>
          replyTo ! (botController.myCurrentRank, botController.grid.frameCount)
          Behaviors.same

        case Observation(obs) =>
          observation = obs
          if(BotServer.streamSender.nonEmpty){
            BotServer.streamSender.get ! GrpcStreamSender.NewObservation(observation._1, observation._2, botController.myCurrentRank, observation._3, observation._4)
          }
          Behaviors.same

        case Dead =>
          log.info(s"switch to dead behavior")
          dead(actor, botController, playerInfo)

        case LeaveRoom=>
          log.info(s"player:${playerInfo.id} leave room, botActor stop.")
          Behaviors.stopped

        case GetFrame(replyTo) =>
          replyTo ! botController.grid.frameCount
          Behaviors.same

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown")
          Behaviors.unhandled
      }
    }
  }

  def waitingForRoomId(actor: ActorRef[Protocol.WsSendMsg],
           botController: BotController,
           playerInfo: PlayerInfoInClient,
           replyTo: ActorRef[String])(implicit stashBuffer: StashBuffer[Command],
                                      timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case RoomId(roomId) =>
          replyTo ! roomId
          stashBuffer.unstashAll(ctx, gaming(actor, botController, playerInfo))

        case unknown@_ =>
          stashBuffer.stash(unknown)
          Behaviors.same
      }
    }
  }

//  def waitingForObservation(actor: ActorRef[Protocol.WsSendMsg],
//                            botController: BotController,
//                            playerInfo: PlayerInfoInClient,
//                            replyTo: ActorRef[(Option[ImgData], Option[LayeredObservation], Int, Boolean)])(
//    implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
//    Behaviors.receive[Command] { (ctx, msg) =>
//      msg match {
//        case Observation(obs) =>
//          replyTo ! obs
//          stashBuffer.unstashAll(ctx, gaming(actor, botController, playerInfo))
//
//        case unknown@_ =>
//          stashBuffer.stash(unknown)
//          Behaviors.same
//      }
//    }
//  }

//  def waitingForObservationWithInfo(actor: ActorRef[Protocol.WsSendMsg],
//                            botController: BotController,
//                            playerInfo: PlayerInfoInClient,
//                            replyTo: ActorRef[(Option[ImgData], Option[LayeredObservation], Score, Int, Boolean)])(
//                             implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
//    Behaviors.receive[Command] { (ctx, msg) =>
//      msg match {
//        case Observation(obs) =>
//          replyTo ! (obs._1, obs._2, botController.myCurrentRank, obs._3, obs._4)
//          stashBuffer.unstashAll(ctx, gaming(actor, botController, playerInfo))
//
//        case unknown@_ =>
//          stashBuffer.stash(unknown)
//          Behaviors.same
//      }
//    }
//  }

  def dead(actor: ActorRef[Protocol.WsSendMsg],
           botController: BotController,
           playerInfo: PlayerInfoInClient)(implicit stashBuffer: StashBuffer[Command],
                                           timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case Reincarnation(replyTo) =>
          actor ! PressSpace
          replyTo ! SimpleRsp(state = State.in_game, msg = "ok")
          //          botController.startGameLoop()
          botController.grid.cleanSnakeTurnPoint(playerInfo.id)
          botController.grid.actionMap = botController.grid.actionMap.filterNot(_._2.contains(playerInfo.id))
          log.info(s"recv msg:$msg,frame:${botController.grid.frameCount}")
          gaming(actor, botController, playerInfo)

        case Action(move, replyTo) =>
          replyTo ! -2L
          Behaviors.same

        case Observation(obs) =>
          if(BotServer.streamSender.nonEmpty){
            BotServer.streamSender.get ! GrpcStreamSender.NewObservation(obs._1, obs._2, botController.myCurrentRank, obs._3, obs._4)
          }
          Behaviors.same

        case ReturnObservation(replyTo) =>
          replyTo ! (None, None, botController.grid.frameCount, false)
          Behaviors.same

        case ReturnObservationWithInfo(replyTo) =>
          log.debug("rec ReturnObservationWithInfo when dead!!!!")
          replyTo ! (None, None, botController.myCurrentRank, botController.grid.frameCount, false)
          Behaviors.same

        case GetFrame(replyTo) =>
          replyTo ! botController.grid.frameCount
          Behaviors.same

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown when dead")
          Behaviors.same
      }
    }
  }

  private def busy()(
    implicit stashBuffer:StashBuffer[Command],
    timer:TimerScheduler[Command]
  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case SwitchBehavior(name, behavior,durationOpt,timeOut) =>
          log.debug(s"switchBehavior")
          switchBehavior(ctx,name,behavior,durationOpt,timeOut)

        case TimeOut(m) =>
          log.debug(s"${ctx.self.path} is time out when busy,msg=${m}")
          Behaviors.stopped

        case unknowMsg =>
          stashBuffer.stash(unknowMsg)
          Behavior.same
      }
    }

  private[this] def getSink(botController: BotController) =
    Sink.foreach[Message] {
      case TextMessage.Strict(msg) =>
        log.debug(s"msg from webSocket: $msg")

      case BinaryMessage.Strict(bMsg) =>
        //decode process.
        val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
        bytesDecode[Protocol.GameMessage](buffer) match {
          case Right(v) => botController.gameMessageReceiver(v)
          case Left(e) =>
            println(s"${System.currentTimeMillis()} decode error1: ${e.message}")
        }

      case msg:BinaryMessage.Streamed => //分片
        val f = msg.dataStream.runFold(new ByteStringBuilder().result()){
          case (s, str) => s.++(str)
        }

        f.map { bMsg =>
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          bytesDecode[Protocol.GameMessage](buffer) match {
            case Right(v) => botController.gameMessageReceiver(v)
            case Left(e) =>
              println(s"decode error2: ${e.message}")
          }
        }

      case unknown@_ =>
        log.debug(s"i receiver an unknown message:$unknown")
    }

  private[this] def getSource = ActorSource.actorRef[WsSendMsg](
    completionMatcher = {
      case WsSendComplete =>
    }, failureMatcher = {
      case WsSendFailed(ex) ⇒ ex
    },
    bufferSize = 64,
    overflowStrategy = OverflowStrategy.fail
  ).collect {
    case message: UserAction =>
      val sendBuffer = new MiddleBufferInJvm(409600)
      BinaryMessage.Strict(ByteString(
        message.fillMiddleBuffer(sendBuffer).result()
      ))
  }

  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String, behavior: Behavior[Command], durationOpt: Option[FiniteDuration] = None, timeOut: TimeOut = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer: TimerScheduler[Command]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey, timeOut, _))
    stashBuffer.unstashAll(ctx, behavior)
  }

  def getJoinRoomWebSocketUri(domain: String, roomId: String, playerId: String, name: String, accessCode: String): String = {
  val wsProtocol = "ws"
//    val domain = "10.1.29.250:30368"
    //    val domain = "localhost:30368"
    s"$wsProtocol://$domain/carnie/joinGame4Client?id=$playerId&name$name&accessCode=$accessCode&mode=1&img=1&roomId=$roomId"
  }

  def getCreateRoomWebSocketUri(domain: String, playerId: String, name: String, accessCode: String, pwd: String): String = {
    val wsProtocol = "ws"
//    val domain = "flowdev.neoap.com"

    //    val domain = "10.1.29.250:30368"
    //    val domain = "localhost:30368"
    s"$wsProtocol://$domain/carnie/joinGame4ClientCreateRoom?id=$playerId&name=$name&accessCode=$accessCode&mode=1&img=1&pwd=$pwd"
  }

}
