package org.seekloud.yubel.http

import java.net.URLDecoder

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorAttributes, Materializer, Supervision}
import akka.util.{ByteString, Timeout}
import org.seekloud.yubel.paperClient.Protocol
import akka.stream.scaladsl.Flow
import org.seekloud.yubel.core.{GameReplay, RoomManager, TokenActor}
import org.seekloud.yubel.paperClient.Protocol._
import org.slf4j.LoggerFactory
import org.seekloud.yubel.Boot.roomManager
import org.seekloud.yubel.common.AppSettings
import org.seekloud.yubel.core.TokenActor.AskForToken
import org.seekloud.yubel.ptcl.{ErrorRsp, SuccessRsp}
import org.seekloud.utils.{CirceSupport, EsheepClient, SessionSupport}
import io.circe.generic.auto._
import akka.actor.typed.scaladsl.AskPattern._
import org.seekloud.yubel.Boot.scheduler
import org.seekloud.yubel.http.SessionBase.{UserInfo, UserSession}

import scala.concurrent.{ExecutionContextExecutor, Future}

/**
  * User: Taoz
  * Date: 9/1/2016
  * Time: 4:13 PM
  */
trait PlayerService extends ServiceUtils with CirceSupport with SessionSupport with SessionBase{

  implicit val system: ActorSystem

  implicit def executor: ExecutionContextExecutor

  val tokenActor: akka.actor.typed.ActorRef[TokenActor.Command]

  implicit val materializer: Materializer

  implicit val timeout: Timeout

  private[this] val log = LoggerFactory.getLogger("org.seekloud.hiStream.http.PlayerService")


  val netSnakeRoute = {
    path("join") {
      parameter(
        'id.as[String],
        'name.as[String],
        'mode.as[Int],
        'img.as[Int]
      ) { (id, name, mode, img) =>
        dealFutureResult {
          val msg: Future[Boolean] = roomManager ? (RoomManager.JudgePlaying(id, _))
          msg.map{r=>
            if(r)
              getFromResource("html/errPage.html")
            else {
              handleWebSocketMessages(webSocketChatFlow(id, name, mode, img))
            }
          }
        }
      }
    } ~
    path("addSession") {
      parameter(
        'id.as[String]
      ) { id =>
        setSession(
          UserSession(UserInfo(id), System.currentTimeMillis()).toUserSessionMap
        ) { ctx =>
          ctx.complete(SuccessRsp())
        }
      }
    } ~
    path("reJoin") {
      parameter(
        'id.as[String],
        'name.as[String],
        'mode.as[Int],
        'img.as[Int]
      ) { (id, name, mode, img) =>
        dealFutureResult {
          val msg: Future[Boolean] = roomManager ? (RoomManager.JudgePlaying(id, _))
          msg.map{r=>
            if(r)
              getFromResource("html/errPage.html")
            else {
              userAuth{ _ =>
                handleWebSocketMessages(webSocketChatFlow(id, name, mode, img))
              }
//              handleWebSocketMessages(webSocketChatFlow(id, name, mode, img))
            }
          }
        }
      }
    }~ path("createRoom") {
      parameter(
        'id.as[String],
        'name.as[String],
        'mode.as[Int],
        'img.as[Int],
        'pwd.as[String]
      ) { (id, name, mode, img, pwd) =>
        dealFutureResult {
          val msg: Future[Boolean] = roomManager ? (RoomManager.JudgePlaying(id, _))
          msg.map{r=>
            if(r)
              getFromResource("html/errPage.html")
            else{
              if(pwd=="")
                handleWebSocketMessages(webSocketChatFlow4CreateRoom(id, name, mode, img, None))
              else
                handleWebSocketMessages(webSocketChatFlow4CreateRoom(id, name, mode, img, Some(pwd)))
            }
          }
        }
      }
    } ~ path("observeGame") {
        parameter(
          'roomId.as[Int],
          'playerId.as[String],
          'accessCode.as[String]
        ) { (roomId, playerId, accessCode) =>
          val gameId = AppSettings.esheepGameId
          dealFutureResult{
            val msg: Future[String] = tokenActor ? AskForToken
            msg.map {token =>
              dealFutureResult{
                log.info("Start to watchGame.")
                EsheepClient.verifyAccessCode(gameId, accessCode, token).map {
                  case Right(data) =>
                    dealFutureResult {
                      val msg: Future[Boolean] = roomManager ? (RoomManager.JudgePlaying4Watch(roomId, data.playerId, _))
                      msg.map{r=>
                        if(r)
                          getFromResource("html/errPage.html")
                        else
                          handleWebSocketMessages(webSocketChatFlow4WatchGame(roomId, playerId, data.playerId))
                      }
                    }
                  case Left(e) =>
                    log.error(s"watchGame error. fail to verifyAccessCode err: $e")
                    complete(ErrorRsp(120003, "Some errors happened in parse verifyAccessCode."))
                }
              }
            }
          }
        }
      } ~ path("joinWatchRecord") {
      parameter(
        'recordId.as[Long],
        'playerId.as[String],
        'frame.as[Int],
        'accessCode.as[String]
      ) { (recordId, playerId, frame, accessCode) =>
        log.debug(s"receive accessCode:$accessCode:")
        val gameId = AppSettings.esheepGameId
        dealFutureResult {
          val msg: Future[String] = tokenActor ? AskForToken
          msg.map { token =>
            dealFutureResult{
              EsheepClient.verifyAccessCode(gameId, accessCode, token).map {
                case Right(rsp) =>
                  handleWebSocketMessages(webSocketChatFlow4WatchRecord(playerId, recordId, frame, rsp.playerId))
                case Left(e) =>
                  complete(ErrorRsp(120006, "Some errors happened in parse verifyAccessCode."))
              }
            }
          }
        }
      }
    } ~
      (path("joinGame4Client") & get ) {
        parameter(
          'id.as[String],
          'name.as[String],
          'accessCode.as[String],
          'mode.as[Int].?,
          'img.as[Int],
          'roomId.as[Int].?
        ) { (id, name, accessCode, mode, img, roomId) =>
          val gameId = AppSettings.esheepGameId
          dealFutureResult{
            val msg: Future[String] = tokenActor ? AskForToken
            msg.map {token =>
              dealFutureResult{
                val playerName = URLDecoder.decode(name, "UTF-8")
                EsheepClient.verifyAccessCode(gameId, accessCode, token).map {
                  case Right(_) =>
                    if(roomId.nonEmpty)
                      handleWebSocketMessages(webSocketChatFlow4JoinRoom(id, playerName, img, roomId.get))
                    else
                      handleWebSocketMessages(webSocketChatFlow(id, playerName, mode.get, img))
                  case Left(e) =>
                    log.error(s"playGame error. fail to verifyAccessCode4Client: $e")
                    complete(ErrorRsp(120010, "Some errors happened in parse verifyAccessCode."))
//                    handleWebSocketMessages(webSocketChatFlow(id, playerName, mode, img))
                }
              }
            }
          }
        }
      } ~
      (path("joinGameById") & get ) {
        parameter(
          'id.as[String],
          'name.as[String],
          'mode.as[Int].?,
          'img.as[Int],
          'roomId.as[Int].?
        ) { (id, name, mode, img, roomId) =>
//          dealFutureResult{
            if(roomId.nonEmpty)
              handleWebSocketMessages(webSocketChatFlow4JoinRoom(id, name, img, roomId.get))
            else
              handleWebSocketMessages(webSocketChatFlow(id, name, mode.get, img))
//            val msg: Future[Boolean] = roomManager ? (RoomManager.JudgePlaying(id, _))
//            msg.map{r=>
//              if(r)
//                getFromResource("html/errPage.html")
//              else{
//                if(roomId.nonEmpty)
//                  handleWebSocketMessages(webSocketChatFlow2(id, name, img, roomId.get))
//                else
//                  handleWebSocketMessages(webSocketChatFlow(id, name, mode.get, img))
//              }
//            }
//          }
        }
      } ~
      (path("joinGame4ClientCreateRoom") & get ) {
        parameter(
          'id.as[String],
          'name.as[String],
          'accessCode.as[String],
          'mode.as[Int],
          'img.as[Int],
          'pwd.as[String]
        ) { (id, name, accessCode, mode, img, pwd) =>
          val gameId = AppSettings.esheepGameId
          dealFutureResult{
            val msg: Future[String] = tokenActor ? AskForToken
            msg.map {token =>
              dealFutureResult{
                val playerName = URLDecoder.decode(name, "UTF-8")
                val newPwd = if(pwd=="") None else Some(pwd)
                EsheepClient.verifyAccessCode(gameId, accessCode, token).map {
                  case Right(_) =>
                    handleWebSocketMessages(webSocketChatFlow4CreateRoom(id, playerName, mode, img, newPwd))
                  case Left(e) =>
                    log.error(s"playGame error. fail to verifyAccessCode4ClientCreateRoom: $e")
                    //                    complete(ErrorRsp(120010, "Some errors happened in parse verifyAccessCode."))
                    handleWebSocketMessages(webSocketChatFlow4CreateRoom(id, playerName, mode, img, newPwd))
                }
              }
            }
          }
        }
      }
  }

  def webSocketChatFlow4WatchGame(roomId: Int, playerId: String, userId: String): Flow[Message, Message, Any] = {
    import scala.language.implicitConversions
    import org.seekloud.byteobject.ByteObject._
    import org.seekloud.byteobject.MiddleBufferInJvm
    import io.circe.generic.auto._
    import io.circe.parser._
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) =>
          log.debug(s"msg from webSocket: $msg")
          TextInfo(msg)

        case BinaryMessage.Strict(bMsg) =>
          //decode process.
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          val msg =
            bytesDecode[UserAction](buffer) match {
              case Right(v) => v
              case Left(e) =>
                println(s"decode error: ${e.message}")
                TextInfo("decode error")
            }
          msg
        // unpack incoming WS text messages...
        // This will lose (ignore) messages not received in one chunk (which is
        // unlikely because chat messages are small) but absolutely possible
        // FIXME: We need to handle TextMessage.Streamed as well.
      }
      .via(RoomManager.watchGame(roomManager, roomId, playerId, userId))
      .map {
        case msg:Protocol.GameMessage =>
          val sendBuffer = new MiddleBufferInJvm(409600)
          BinaryMessage.Strict(ByteString(
            //encoded process
            msg.fillMiddleBuffer(sendBuffer).result()

          ))

        case x =>
          TextMessage.apply("")

      }.withAttributes(ActorAttributes.supervisionStrategy(decider)) // ... then log any processing errors on stdin
  }

  var snakeAction : Double = 0
  var ping : Double = 0
  var newField : Double = 0
  var data4TotalSync : Double = 0
  var rank : Double = 0
  var newSnakeInfo : Double = 0
  var dead :Double = 0
  var win :Double = 0
  var other :Double = 0
  var updateTime = 0l
  var newData: Double = 0

  def webSocketChatFlow(playedId: String, sender: String, mode: Int, img: Int): Flow[Message, Message, Any] = {
    import scala.language.implicitConversions
    import org.seekloud.byteobject.ByteObject._
    import org.seekloud.byteobject.MiddleBufferInJvm
    import io.circe.generic.auto._
    import io.circe.parser._
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) =>
          log.debug(s"msg from webSocket: $msg")
          TextInfo(msg)

        case BinaryMessage.Strict(bMsg) =>
          //decode process.
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          val msg =
            bytesDecode[UserAction](buffer) match {
              case Right(v) => v
              case Left(e) =>
                println(s"decode error: ${e.message}")
                TextInfo("decode error")
            }
          msg
        // unpack incoming WS text messages...
        // This will lose (ignore) messages not received in one chunk (which is
        // unlikely because chat messages are small) but absolutely possible
        // FIXME: We need to handle TextMessage.Streamed as well.
      }
      .via(RoomManager.joinGame(roomManager, playedId, sender, mode, img))
      .map {
        case msg:Protocol.GameMessage =>
          val sendBuffer = new MiddleBufferInJvm(409600)
          val a = msg.fillMiddleBuffer(sendBuffer).result()
          msg match {
            case ReceivePingPacket(_) =>
              ping = ping + a.length

            case BoardAction(_, _, _, _, _) =>
              snakeAction = snakeAction + a.length

            case OtherAction(_,_,_,_) =>
              snakeAction = snakeAction + a.length

            case NewData(_,_,_) =>
              newData = newData + a.length

            case NewFieldInfo(_, _) =>
              newField = newField + a.length

            case Data4TotalSync(_, _, _, _) =>
              data4TotalSync = data4TotalSync + a.length

            case Ranks(_, _, _, _) =>
              rank = rank + a.length

            case NewSnakeInfo(_, _) =>
              newSnakeInfo = newSnakeInfo + a.length

            case DeadPage(_, _, _) =>
              dead = dead + a.length

            case WinData(_, _, _) =>
              win = win + a.length

            case _ =>
              other = other + a.length
          }
          if(System.currentTimeMillis() - updateTime > 30*1000){
            updateTime = System.currentTimeMillis()
            log.debug(s"statistics!!!!!ping:$ping,snakeAction:$snakeAction,newData:$newData,newField:$newField,data4TotalSync$data4TotalSync,rank:$rank,newSnakeInfo:$newSnakeInfo, dead$dead, win:$win,other:$other")
            snakeAction = 0
            ping = 0
            newField = 0
            data4TotalSync = 0
            rank = 0
            newSnakeInfo = 0
            dead = 0
            win = 0
            other = 0
          }
          BinaryMessage.Strict(ByteString(a))

        case x =>
          println("unknown")
          TextMessage.apply("")

      }.withAttributes(ActorAttributes.supervisionStrategy(decider)) // ... then log any processing errors on stdin
  }

  def webSocketChatFlow4JoinRoom(playedId: String, sender: String, img: Int, roomId: Int): Flow[Message, Message, Any] = {
    import scala.language.implicitConversions
    import org.seekloud.byteobject.ByteObject._
    import org.seekloud.byteobject.MiddleBufferInJvm
    import io.circe.generic.auto._
    import io.circe.parser._
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) =>
          log.debug(s"msg from webSocket: $msg")
          TextInfo(msg)

        case BinaryMessage.Strict(bMsg) =>
          //decode process.
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          val msg =
            bytesDecode[UserAction](buffer) match {
              case Right(v) => v
              case Left(e) =>
                println(s"decode error: ${e.message}")
                TextInfo("decode error")
            }
          msg
        // unpack incoming WS text messages...
        // This will lose (ignore) messages not received in one chunk (which is
        // unlikely because chat messages are small) but absolutely possible
        // FIXME: We need to handle TextMessage.Streamed as well.
      }
      .via(RoomManager.joinGameByRoomId(roomManager, playedId, sender, img, roomId))
      .map {
        case msg:Protocol.GameMessage =>
          val sendBuffer = new MiddleBufferInJvm(409600)
          BinaryMessage.Strict(ByteString(
            //encoded process
            msg.fillMiddleBuffer(sendBuffer).result()

          ))

        case x =>
          TextMessage.apply("")

      }.withAttributes(ActorAttributes.supervisionStrategy(decider)) // ... then log any processing errors on stdin
  }

  def webSocketChatFlow4CreateRoom(playedId: String, sender: String, mode: Int, img: Int, pwd: Option[String]): Flow[Message, Message, Any] = {
    import scala.language.implicitConversions
    import org.seekloud.byteobject.ByteObject._
    import org.seekloud.byteobject.MiddleBufferInJvm
    import io.circe.generic.auto._
    import io.circe.parser._
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) =>
          log.debug(s"msg from webSocket: $msg")
          TextInfo(msg)

        case BinaryMessage.Strict(bMsg) =>
          //decode process.
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          val msg =
            bytesDecode[UserAction](buffer) match {
              case Right(v) => v
              case Left(e) =>
                println(s"decode error: ${e.message}")
                TextInfo("decode error")
            }
          msg
        // unpack incoming WS text messages...
        // This will lose (ignore) messages not received in one chunk (which is
        // unlikely because chat messages are small) but absolutely possible
        // FIXME: We need to handle TextMessage.Streamed as well.
      }
      .via(RoomManager.createRoom(roomManager, playedId, sender, mode, img, pwd))
      .map {
        case msg:Protocol.GameMessage =>
          val sendBuffer = new MiddleBufferInJvm(409600)
          BinaryMessage.Strict(ByteString(
            //encoded process
            msg.fillMiddleBuffer(sendBuffer).result()

          ))

        case x =>
          TextMessage.apply("")

      }.withAttributes(ActorAttributes.supervisionStrategy(decider)) // ... then log any processing errors on stdin
  }

  def webSocketChatFlow4WatchRecord(playedId: String, recordId: Long, frame: Int, playerId: String): Flow[Message, Message, Any] = {
    import scala.language.implicitConversions
    import org.seekloud.byteobject.ByteObject._
    import org.seekloud.byteobject.MiddleBufferInJvm
    import io.circe.generic.auto._
    import io.circe.parser._
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) =>
          log.debug(s"msg from webSocket: $msg")
          TextInfo(msg)

        case BinaryMessage.Strict(bMsg) =>
          //decode process.
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          val msg =
            bytesDecode[UserAction](buffer) match {
              case Right(v) => v
              case Left(e) =>
                println(s"decode error: ${e.message}")
                TextInfo("decode error")
            }
          msg
        // unpack incoming WS text messages...
        // This will lose (ignore) messages not received in one chunk (which is
        // unlikely because chat messages are small) but absolutely possible
        // FIXME: We need to handle TextMessage.Streamed as well.
      }
      .via(RoomManager.replayGame(roomManager, recordId, playedId, frame, playerId))
      .map {
        case msg:Protocol.GameMessage =>
          val sendBuffer = new MiddleBufferInJvm(409600)
          BinaryMessage.Strict(ByteString(
            //encoded process
            msg.fillMiddleBuffer(sendBuffer).result()

          ))

        case x =>
          TextMessage.apply("")

      }.withAttributes(ActorAttributes.supervisionStrategy(decider)) // ... then log any processing errors on stdin
  }


  val decider: Supervision.Decider = {
    e: Throwable =>
      e.printStackTrace()
      println(s"WS stream failed with $e")
      Supervision.Resume
  }


}
