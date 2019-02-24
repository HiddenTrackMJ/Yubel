package org.seekloud.yubel.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import akka.actor.typed.scaladsl.AskPattern._
import org.seekloud.yubel.ptcl.RoomApiProtocol._
import org.seekloud.yubel.core.RoomManager
import org.seekloud.utils.CirceSupport
import org.seekloud.yubel.Boot.{executor, scheduler}
import org.seekloud.yubel.ptcl.RoomApiProtocol
import io.circe.generic.auto._
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import io.circe.Error

import scala.collection.mutable



/**
  * Created by dry on 2018/10/18.
  **/
trait RoomApiService extends ServiceUtils with CirceSupport with PlayerService  {

  private val log = LoggerFactory.getLogger("org.seekloud.carnie.http.RoomApiService")

  val roomManager: akka.actor.typed.ActorRef[RoomManager.Command]

//  private val createRoom = (path("createRoom") & post & pathEndOrSingleSlash) {
//    entity(as[Either[Error, CreateRoomInfo]]) {
//      case Right(r) =>
//        roomManager ! RoomManager.CreateRoom(r.mode, r.pwd)
//        complete(SuccessRsp())
//      case Left(e) =>
//        log.debug(s"got errMsg: $e")
//        complete(ErrorRsp(100020, s"createRoom error: $e"))
//    }
//  }

  private val getRoomId = (path("getRoomId") & post & pathEndOrSingleSlash) {
    dealPostReq[PlayerIdInfo] { req =>
      val msg: Future[Option[Int]] = roomManager ? (RoomManager.FindRoomId(req.playerId, _))
      msg.map {
        case Some(rid) => complete(RoomIdRsp(RoomIdInfo(rid)))
        case _ =>
          log.info("this player doesn't exist")
          complete(ErrorRsp(100010, "get roomId error:this player doesn't exist"))
      }
    }
  }

  // TODO:
  private val getRoomPlayerList = (path("getRoomPlayerList") & post & pathEndOrSingleSlash) {
    dealPostReq[RoomIdReq] { req =>
      val msg: Future[List[PlayerIdName]] = roomManager ? (RoomManager.FindPlayerList(req.roomId, _))
      msg.map { plist =>
        if(plist.nonEmpty){
//          log.info(s"plist:$plist")
          complete(PlayerListRsp(PlayerInfo(plist)))
        }
         else{
          log.info("get player list error: this room doesn't exist")
          complete(ErrorRsp(100001, "get player list error: this room doesn't exist"))
        }

      }
    }
  }


  private val getRoomList = (path("getRoomList") & post & pathEndOrSingleSlash) {
    dealPostReqWithoutData {
      val msg: Future[mutable.HashMap[Int, (Int, Option[String], mutable.HashSet[(String, String)])]] = roomManager ? RoomManager.ReturnRoomMap
      msg.map {
        r =>
          val allRoom = r.keySet.toList
          if (allRoom.nonEmpty){
            log.info("prepare to return roomList.")
            complete(RoomListRsp(RoomListInfo(allRoom)))
          }
          else {
            log.info("get all room error,there are no rooms")
            complete(ErrorRsp(100000, "get all room error,there are no rooms"))
          }
      }
    }
  }

  private val getRoomList4Front = (path("getRoomList4Front") & get & pathEndOrSingleSlash) {
    dealFutureResult {
      val msg: Future[List[String]] = roomManager ? RoomManager.FindAllRoom4Client
      msg.map {
        allRoom =>
            log.info("prepare to return roomList.")
            complete(RoomApiProtocol.RoomListRsp4Client(RoomListInfo4Client(allRoom)))
      }
    }
  }


  private val netSnake = (path("netSnake") & get & pathEndOrSingleSlash){
   getFromResource("html/index.html")
  }

  val roomApiRoutes: Route = {
    getRoomId ~ getRoomList  ~ getRoomPlayerList ~
     getRoomList4Front ~ netSnake
  }


}
