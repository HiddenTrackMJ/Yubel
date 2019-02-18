package org.seekloud.yubel.actor

import java.net.URLEncoder

import akka.actor.typed._
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
import org.slf4j.LoggerFactory
import akka.stream.scaladsl.{Keep, Sink}

import scala.concurrent.Future
import org.seekloud.yubel.controller.GameController
import org.seekloud.yubel.Boot.{executor, materializer, scheduler, system}
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.yubel.paperClient.Protocol

import scala.concurrent.duration.FiniteDuration

/**
  * Created by dry on 2018/10/23.
  **/
object PlayGameWebSocket {

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  private final case object BehaviorChangeKey

  sealed trait WsCommand

  case class ConnectGame(playerInfo: PlayerInfoInClient, domain: String, mode: Int, img: Int) extends WsCommand//mode: Int, img: Int

  case class CreateRoom(playerInfo: PlayerInfoInClient, domain: String, mode: Int, img: Int, pwd: String) extends WsCommand//mode: Int, img: Int

  case class JoinByRoomId(playerInfo: PlayerInfoInClient, domain: String, roomId: Int, img: Int) extends WsCommand//roomId: Int, img: Int

  case class MsgToService(sendMsg: WsSendMsg) extends WsCommand

  case object Terminate extends WsCommand

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[WsCommand],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends WsCommand

  case class TimeOut(msg:String) extends WsCommand

  private[this] def switchBehavior(ctx: ActorContext[WsCommand],
                                   behaviorName: String, behavior: Behavior[WsCommand], durationOpt: Option[FiniteDuration] = None,timeOut: TimeOut  = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[WsCommand],
                                   timer:TimerScheduler[WsCommand]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey,timeOut,_))
    stashBuffer.unstashAll(ctx,behavior)
  }

  def create(gameController: GameController): Behavior[WsCommand] = {
    Behaviors.setup[WsCommand] { ctx =>
      implicit val stashBuffer: StashBuffer[WsCommand] = StashBuffer[WsCommand](Int.MaxValue)
      Behaviors.withTimers { implicit timer =>
        idle(gameController)
      }
    }
  }

  def idle(gameController: GameController)(implicit stashBuffer: StashBuffer[WsCommand], timer: TimerScheduler[WsCommand]): Behavior[WsCommand] = {
    Behaviors.receive[WsCommand] { (ctx, msg) =>
      msg match {
        case ConnectGame(playerInfo, domain, mode, img) =>
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(getWebSocketUri(playerInfo.id, playerInfo.name, playerInfo.accessCode, domain, mode, img)))
          val source = getSource
          val sink = getSink(gameController)
          val ((stream, response), closed) =
            source
              .viaMat(webSocketFlow)(Keep.both) // keep the materialized Future[WebSocketUpgradeResponse]
              .toMat(sink)(Keep.both) // also keep the Future[Done]
              .run()

          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
              Future.successful("connect success")
            } else {
              throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
            }
          } //ws建立

          closed.onComplete { _ =>
            log.info("connect to service closed!")
            gameController.loseConnect()
          } //ws断开
          connected.onComplete(i => log.info(i.toString))
          connecting(stream)

        case CreateRoom(playerInfo, domain, mode, img, pwd) =>
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(getWebSocketUri4CreateRoom(playerInfo.id, playerInfo.name, playerInfo.accessCode, domain, mode, img, pwd)))
          val source = getSource
          val sink = getSink(gameController)
          val ((stream, response), closed) =
            source
              .viaMat(webSocketFlow)(Keep.both) // keep the materialized Future[WebSocketUpgradeResponse]
              .toMat(sink)(Keep.both) // also keep the Future[Done]
              .run()

          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
              Future.successful("connect success")
            } else {
              throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
            }
          } //ws建立

          closed.onComplete { _ =>
            log.info("connect to service closed!")
            gameController.loseConnect()
          } //ws断开
          connected.onComplete(i => log.info(i.toString))
          connecting(stream)

        case JoinByRoomId(playerInfo, domain, roomId, img) =>
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(getWebSocketUri4JoinByRoomId(playerInfo.id, playerInfo.name, playerInfo.accessCode, domain, roomId, img)))
          val source = getSource
          val sink = getSink(gameController)
          val ((stream, response), closed) =
            source
              .viaMat(webSocketFlow)(Keep.both) // keep the materialized Future[WebSocketUpgradeResponse]
              .toMat(sink)(Keep.both) // also keep the Future[Done]
              .run()

          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
              Future.successful("connect success")
            } else {
              throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
            }
          } //ws建立

          closed.onComplete { _ =>
            log.info("connect to service closed!")
            gameController.loseConnect()
          } //ws断开
          connected.onComplete(i => log.info(i.toString))
          connecting(stream)

        case Terminate =>
          Behaviors.stopped

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown")
          Behaviors.unhandled
      }
    }
  }

  def connecting(actor: ActorRef[Protocol.WsSendMsg])(implicit stashBuffer: StashBuffer[WsCommand], timer: TimerScheduler[WsCommand]): Behavior[WsCommand] = {
    Behaviors.receive[WsCommand] { (ctx, msg) =>
      msg match {
        case m@MsgToService(sendMsg) =>
          actor ! sendMsg
          Behaviors.same

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown")
          Behaviors.unhandled
      }
    }
  }

  private[this] def getSink(gameController: GameController) =
    Sink.foreach[Message] {
      case TextMessage.Strict(msg) =>
        log.debug(s"msg from webSocket: $msg")

      case BinaryMessage.Strict(bMsg) =>
        //decode process.
        val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
        bytesDecode[GameMessage](buffer) match {
          case Right(v) => gameController.gameMessageReceiver(v)
          case Left(e) =>
            println(s"decode error: ${e.message}")
        }

      case msg:BinaryMessage.Streamed =>
        val f = msg.dataStream.runFold(new ByteStringBuilder().result()){
          case (s, str) => s.++(str)
        }

        f.map { bMsg =>
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          bytesDecode[GameMessage](buffer) match {
            case Right(v) => gameController.gameMessageReceiver(v)
            case Left(e) =>
              println(s"decode error: ${e.message}")
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

  def getWebSocketUri(playerId: String, playerName: String, accessCode: String, domain: String, mode: Int, img: Int): String = {
    val wsProtocol = "ws"
//    val domain = "10.1.29.250:30368"
//    println(s"domain: $domain")
    //    val domain = "localhost:30368"
    val name = URLEncoder.encode(playerName, "UTF-8")
    s"$wsProtocol://$domain/carnie/joinGame4Client?id=$playerId&name=$name&accessCode=$accessCode&mode=$mode&img=$img"
  }

  def getWebSocketUri4CreateRoom(playerId: String, playerName: String, accessCode: String, domain: String, mode: Int, img: Int, pwd: String): String = {
    val wsProtocol = "ws"
    val name = URLEncoder.encode(playerName, "UTF-8")
    s"$wsProtocol://$domain/carnie/joinGame4ClientCreateRoom?id=$playerId&name=$name&accessCode=$accessCode&mode=$mode&img=$img&pwd=$pwd"
  }

  def getWebSocketUri4JoinByRoomId(playerId: String, playerName: String, accessCode: String, domain: String, roomId: Int, img: Int): String = {
    val wsProtocol = "ws"
    val name = URLEncoder.encode(playerName, "UTF-8")
    s"$wsProtocol://$domain/carnie/joinGame4Client?id=$playerId&name=$name&accessCode=$accessCode&img=$img&roomId=$roomId"
  }

}
