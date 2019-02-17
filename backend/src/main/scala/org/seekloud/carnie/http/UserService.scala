package org.seekloud.carnie.http

import akka.http.scaladsl.server.Directives.{as, complete, entity, getFromResource, path, pathEndOrSingleSlash, pathPrefix}

import org.seekloud.utils.{CirceSupport, SessionSupport}
import org.slf4j.LoggerFactory
import akka.http.scaladsl.server.Directives._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.http.scaladsl.server.Route
import io.circe.generic.auto._
import io.circe.Error
import org.seekloud.carnie.http.SessionBase.{AdminInfo, AdminSession}

import org.seekloud.carnie.models.dao.UserDAO
import org.seekloud.carnie.ptcl.UserPtcl
import org.seekloud.carnie.ptcl.UserPtcl._

/**
  * User: Jason
  * Date: 2019/2/17
  * Time: 14:29
  */
trait UserService extends ServiceUtils
  with CirceSupport
  with SessionBase
  with SessionSupport{

  private val log = LoggerFactory.getLogger(this.getClass)


  private val login = (path("login") & post & pathEndOrSingleSlash){
    entity(as[Either[Error, UserPtcl.LoginReq]]) {
      case Right(req) =>
        dealFutureResult{
          UserDAO.getIdPwdState(req.name).map{
            case Some(p) =>
              if (p._2 == req.passWord && p._3 == 0) complete(UserPtcl.IdRsp(p._1))
              else if (p._2 != req.passWord) complete(IdRsp(1,140015, s"Password is wrong."))
              else complete(IdRsp(1,140015, s"This account is disabled."))
            case None =>
              complete(IdRsp(1,140012, s"This account doesn't exists."))
            case _ =>
              complete(IdRsp(1,140013, s"Unknown errors happened when login"))
          }.recover {
            case e: Exception =>
              log.info(s"getPwd exception.." + e.getMessage)
              complete(IdRsp(1,130019, "getPwd error."))
          }
        }
      case Left(e) =>
        complete(IdRsp(1,140004, s"Some errors happened when login：$e"))
    }
  }

  private val signUp = (path("signUp") & post & pathEndOrSingleSlash){
    entity(as[Either[Error, UserPtcl.AddUserReq]]) {
      case Right(req) =>
        dealFutureResult{
          UserDAO.isExist(req.userName).map{ b =>
            if (b) {
              log.info("This name already exists!")
              complete(ErrorRsp(140011, "This name already exists!"))
            }
            else {
              UserDAO.addUser(req.userName,req.securePwd,System.currentTimeMillis(),0)
              complete(UserPtcl.SuccessRsp())
            }
          }
        }

      case Left(e) =>
        complete(ErrorRsp(140010, s"Some errors happened when signing up：$e"))
    }
  }


  val userRoutes: Route = {
    login ~ signUp
  }
}
