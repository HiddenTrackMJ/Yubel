package org.seekloud.yubel.actor

import akka.Done
import akka.actor.typed._
import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage, WebSocketRequest}
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Keep, Sink}
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import akka.util.ByteString
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.byteobject.ByteObject.{bytesDecode, _}
import org.seekloud.byteobject.MiddleBufferInJvm
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContextExecutor, Future}
import org.seekloud.yubel.common.{AppSetting, Context}
import org.seekloud.yubel.controller.{GameController, LoginController}
import org.seekloud.yubel.Boot.{executor, materializer, scheduler, system}
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.yubel.utils.Api4GameAgent.linkGameAgent

/**
  * Created by dry on 2018/10/23.
  **/
object LoginSocketClient {

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  sealed trait WsCommand

  case class EstablishConnection2Es(wsUrl: String) extends WsCommand

  case class Connection2EsByMail(userId: Long, playerName:String, token: String) extends WsCommand

  def create(context: Context, loginController: LoginController): Behavior[WsCommand] = {
    Behaviors.setup[WsCommand] { ctx =>
      Behaviors.withTimers { implicit timer =>
        idle(context, loginController)(timer)
      }
    }
  }

  def idle(context: Context, loginController: LoginController)(implicit timer: TimerScheduler[WsCommand]): Behavior[WsCommand] = {
    Behaviors.receive[WsCommand] { (ctx, msg) =>
      msg match {
        case EstablishConnection2Es(wsUrl: String) =>
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(wsUrl))

          val source = getSource
          val sink = getSink4EstablishConnection(ctx.self, context, loginController)
          val response =
            source
              .viaMat(webSocketFlow)(Keep.right)
              .toMat(sink)(Keep.left)
              .run()
          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {

              Future.successful("WsClient connect success. EstablishConnectionEs!")
            } else {
              throw new RuntimeException(s"WSClient connection failed: ${upgrade.response.status}")
            }
          } //链接建立时
//          connected.onComplete(i => log.info(i.toString))
          Behavior.same

        case Connection2EsByMail(userId:Long, playerName:String, token:String) =>
//          val gameId = AppSetting.esheepGameId
          val playerId = "user" + userId
          loginController.switchToSelecting(PlayerInfoInClient(playerId, playerName, token))//domain,ip,port

//          linkGameAgent(gameId, playerId, token).map {
//            case Right(r) =>
//              loginController.switchToSelecting(PlayerInfoInClient(playerId, playerName, r.accessCode), r.gsPrimaryInfo.domain)//domain,ip,port
//
//            case Left(e) =>
//              log.debug(s"linkGameAgent..$e")
////              loginController.switchToSelecting(PlayerInfoInClient(playerId, playerName, "test"), "test")//domain,ip,port
//
//          }
          Behaviors.same
      }
    }
  }

  def getSink4EstablishConnection(self: ActorRef[WsCommand], context: Context, loginController: LoginController):Sink[Message,Future[Done]] = {
    Sink.foreach {
      case TextMessage.Strict(msg) =>
        import io.circe.generic.auto._
        import io.circe.parser.decode
        import org.seekloud.yubel.protocol.Protocol4Agent._
        import org.seekloud.yubel.utils.Api4GameAgent.linkGameAgent
        import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
        import org.seekloud.yubel.Boot.executor

        decode[WsData](msg) match {
          case Right(res) =>
            res match {
              case Ws4AgentRsp(data, errCode, errMsg) =>
                if (errCode != 0) {
                  log.debug(s"receive responseRsp error....$errMsg")
                } else {
                  val playerId = "user" + data.userId.toString
                  val playerName = data.nickname
                  loginController.switchToSelecting(PlayerInfoInClient(playerId, playerName, data.token))//domain,ip,port
//                  linkGameAgent(gameId, playerId, data.token).map {
//                    case Right(r) =>
//                      loginController.switchToSelecting(PlayerInfoInClient(playerId, playerName, r.accessCode), r.gsPrimaryInfo.domain)//domain,ip,port
//
//                    case Left(e) =>
//                      log.debug(s"linkGameAgent..$e")
//                  }
                }

              case HeartBeat =>
                //收到心跳消息不做处理
            }

          case Left(e) =>
            log.debug(s"decode esheep webmsg error! Error information:$e")
        }

      case unknown@_ =>
        log.debug(s"i receive an unknown msg:$unknown")
    }
  }

  private[this] def getSource = ActorSource.actorRef[WsSendMsg](
    completionMatcher = {
      case WsSendComplete =>
    }, failureMatcher = {
      case WsSendFailed(ex) ⇒ ex
    },
    bufferSize = 8,
    overflowStrategy = OverflowStrategy.fail
  ).collect {
    case message: UserAction =>
      val sendBuffer = new MiddleBufferInJvm(409600)
      BinaryMessage.Strict(ByteString(
        message.fillMiddleBuffer(sendBuffer).result()
      ))

  }

}
