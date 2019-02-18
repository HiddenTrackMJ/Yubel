package org.seekloud.yubel.http

import java.text.SimpleDateFormat

import akka.http.scaladsl.server.Directives.{getFromResource, path}
import akka.http.scaladsl.server.Route
import org.seekloud.utils.{CirceSupport, SessionSupport}
import org.slf4j.LoggerFactory
import akka.http.scaladsl.server.Directives._
import org.seekloud.yubel.models.dao.UserDAO
import org.seekloud.yubel.ptcl.AdminPtcl.{PageReq, PageTimeReq}
import org.seekloud.yubel.ptcl.RoomApiProtocol._
import org.seekloud.yubel.ptcl.UserPtcl.ErrorRsp
import org.seekloud.yubel.ptcl.{AdminPtcl, UserPtcl}

import scala.collection.mutable
//import akka.http.scaladsl.model.{ContentTypes, DateTime, HttpEntity}
import akka.http.scaladsl.server.{Directive1, Route}
import org.seekloud.yubel.core.RoomManager
import io.circe.generic.auto._
import io.circe.Error
import org.seekloud.yubel.http.SessionBase.{AdminInfo, AdminSession}
import akka.actor.typed.scaladsl.AskPattern._

import scala.concurrent.Future
import org.seekloud.yubel.Boot.{executor, scheduler}
import org.seekloud.yubel.common.AppSettings
import org.seekloud.yubel.models.dao.PlayerRecordDAO
//import scala.util.{Failure, Success}

/**
  * User: Jason
  * Date: 2018/12/17
  * Time: 16:29
  */
trait AdminService extends ServiceUtils
  with CirceSupport
  with SessionBase
  with SessionSupport
  with RoomApiService
{

  private val log = LoggerFactory.getLogger(this.getClass)


  private val login = (path("login") & post & pathEndOrSingleSlash){
    entity(as[Either[Error, AdminPtcl.LoginReq]]) {
      case Right(req) =>
        if(AppSettings.adminId == req.id && AppSettings.adminPassWord == req.passWord){
          setSession(
            AdminSession(AdminInfo(req.id, req.passWord), System.currentTimeMillis()).toAdminSessionMap
          ) { ctx =>
            ctx.complete(SuccessRsp())
          }
        }
        else {
          log.info("Administrator's account or password is wrong!")
          complete(ErrorRsp(140001, "Administrator's account or password is wrong!"))
        }
      case Left(e) =>
        complete(ErrorRsp(140001, s"Some errors happened in adminLogin：$e"))
    }
  }

//  private val getRoomList = (path("getRoomList") & get & pathEndOrSingleSlash){
//    adminAuth{ _ =>
//      dealFutureResult {
//        val msg: Future[List[String]] = roomManager ? RoomManager.FindAllRoom4Client
//        msg.map {
//          allRoom =>
//            log.info("prepare to return roomList.")
//            complete(RoomApiProtocol.RoomListRsp4Client(RoomListInfo4Client(allRoom)))
//        }
//      }
//    }
//  }

  private val getRoomPlayerList = (path("getRoomPlayerList") & get & pathEndOrSingleSlash) {
    adminAuth {
      _ =>
        //    val msg: Future[List[PlayerIdName]] = roomManager ? (RoomManager.FindPlayerList(req.roomId, _))
        val msg: Future[mutable.HashMap[Int, (Int, Option[String], mutable.HashSet[(String, String)])]] = roomManager ? RoomManager.ReturnRoomMap
        dealFutureResult {
          msg.map { plist =>
            //        val plist =r.map(i => i._1 -> i._2._3)
            complete(RoomMapRsp(RoomMapInfo(plist)))
          }
        }
    }
  }

  private val getPlayerRecord = (path("getPlayerRecord") & post & pathEndOrSingleSlash) {
    adminAuth {
      _ =>
        entity(as[Either[Error, PageReq]]) {
          case Right(req) =>
            val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
            dealFutureResult {
              PlayerRecordDAO.getPlayerRecord().map { p =>
                complete(AdminPtcl.PlayerRecordRsp(p.toList.map { i =>
                  AdminPtcl.PlayerRecord(i.id, i.playerId, i.nickname, i.killing, i.killed,
                    i.score, i.startTime, i.endTime)
                }.slice((req.page - 1) * 5, req.page * 5), p.length
                ))
              }.recover {
                case e: Exception =>
                  log.info(s"getPlayerRecord exception.." + e.getMessage)
                  complete(ErrorRsp(130019, "getPlayerRecord error."))
              }
            }
          case Left(_) =>
            complete(ErrorRsp(130026, "parse error."))
        }
    }
  }

  private val getPlayerRecordAmount = (path("getPlayerRecordAmount") & get & pathEndOrSingleSlash) {
    adminAuth {
      _ =>
        val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
        val date = dateFormat.format(System.currentTimeMillis())
        val start = date.take(11) + "00:00:00"
        val end = date.take(11) + "23:59:59"
        dealFutureResult {
          PlayerRecordDAO.getPlayerRecord().map { p =>
            complete(AdminPtcl.PlayerAmountRsp(p.map(_.playerId).distinct.length,
              p.filter(i => dateFormat.format(i.startTime) >= start && dateFormat.format(i.endTime) <= end).map(_.playerId).distinct.length))
          }.recover {
            case e: Exception =>
              log.info(s"getPlayerRecordAmount exception.." + e.getMessage)
              complete(ErrorRsp(130020, "getPlayerRecordAmount error."))
          }
        }
    }
  }

  private val getPlayerRecordByTime = (path("getPlayerRecordByTime") & post & pathEndOrSingleSlash) {
    adminAuth {
      _ =>
        entity(as[Either[Error, PageTimeReq]]) {
          case Right(req) =>
            val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
            val startS = req.time.take(10) + " 00:00:00"
            val endS = req.time.take(10) + " 23:59:59"
            dealFutureResult {
              PlayerRecordDAO.getPlayerRecord().map { p =>
                complete(AdminPtcl.PlayerRecordRsp(p.toList.filter(i => dateFormat.format(i.startTime) >= startS && dateFormat.format(i.endTime) <= endS).map { i =>
                  AdminPtcl.PlayerRecord(i.id, i.playerId, i.nickname, i.killing, i.killed, i.score, i.startTime, i.endTime)
                }.slice((req.page - 1) * 5, req.page * 5),
                  p.count(i => dateFormat.format(i.startTime) >= startS && dateFormat.format(i.endTime) <= endS)))
              }.recover {
                case e: Exception =>
                  log.info(s"getPlayerRecordByTime exception.." + e.getMessage)
                  complete(ErrorRsp(130019, "getPlayerRecordByTime error."))
              }
            }
          case Left(_) =>
            complete(ErrorRsp(130026, "parse error."))
        }
    }
  }

  private val getPlayerByTimeAmount = (path("getPlayerByTimeAmount") & post & pathEndOrSingleSlash) {
    adminAuth {
      _ =>
        entity(as[Either[Error, AdminPtcl.TimeReq]]) {
          case Right(req) =>
            val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
            val startS = req.time.take(10) + " 00:00:00"
            val endS = req.time.take(10) + " 23:59:59"
            dealFutureResult {
              PlayerRecordDAO.getPlayerRecord().map { p =>
                complete(AdminPtcl.PlayerByTimeAmountRsp(p.filter(i => dateFormat.format(i.startTime) >= startS &&
                  dateFormat.format(i.endTime) <= endS).map(_.playerId).distinct.length))
              }.recover {
                case e: Exception =>
                  log.info(s"getPlayerByTimeAmount exception.." + e.getMessage)
                  complete(ErrorRsp(130021, "getPlayerByTimeAmount error."))
              }
            }
          case Left(_) =>
            complete(ErrorRsp(130026, "parse error."))
        }
    }
  }

  private val logout = path("logout") {
    adminAuth {
      _ =>
        invalidateSession {
          complete(SuccessRsp())
        }
    }
  }

  private val deleteUser = (path("deleteUser") & post & pathEndOrSingleSlash){
    adminAuth {
      _ =>
        entity(as[Either[Error, AdminPtcl.DeleteUserReq]]) {
          case Right(req) =>
            dealFutureResult{
              UserDAO.isExist(req.userName).map{ b =>
                if (b) {
                  UserDAO.deleteUser(req.userName)
                  complete(AdminPtcl.SuccessRsp())
                }
                else {
                  log.info("This name doesn't exists!")
                  complete(ErrorRsp(140011, "This name doesn't exists!"))
                }
              }
            }

          case Left(e) =>
            complete(ErrorRsp(140010, s"Some errors happened when deleting user：$e"))
        }
    }
  }

  private val updateUser = (path("updateUser") & post & pathEndOrSingleSlash){
   adminAuth {
     _ =>
       entity(as[Either[Error, AdminPtcl.UpdateUserReq]]) {
         case Right(req) =>
           dealFutureResult{
             UserDAO.isExist(req.userName).map{ b =>
               if (b) {
                 UserDAO.updateUser(req.userName,req.securePwd,req.state)
                 complete(AdminPtcl.SuccessRsp())
               }
               else {
                 log.info("This name doesn't exists!")
                 complete(ErrorRsp(140011, "This name doesn't exists!"))
               }
             }
           }

         case Left(e) =>
           complete(ErrorRsp(140010, s"Some errors happened when updating user：$e"))
       }
   }
  }

  private val getAllUser = (path("getAllUser") & get & pathEndOrSingleSlash){
    adminAuth {
      _ =>
        dealFutureResult {
          UserDAO.getAllUser.map{ u =>
            complete(AdminPtcl.AllUserRsp(u.toList.map( a =>
              AdminPtcl.UserInfo(a.id,a.username,a.securePwd,a.createTime,a.state))))
          }.recover{
            case e: Exception =>
              log.info(s"getAllUsers exception.." + e.getMessage)
              complete(ErrorRsp(130021, "getAllUsers error."))
          }
        }

    }
  }

  val adminRoutes: Route = pathPrefix("admin"){
    pathEndOrSingleSlash {
      getFromResource("html/admin.html")
    } ~
    login ~ logout ~ getRoomPlayerList ~ getPlayerRecord ~ getPlayerRecordByTime ~ getPlayerRecordAmount ~
      getPlayerByTimeAmount ~ deleteUser ~ updateUser ~ getAllUser
  }
}
