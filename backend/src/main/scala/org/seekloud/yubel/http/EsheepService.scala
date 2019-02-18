package org.seekloud.yubel.http

import org.slf4j.LoggerFactory
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import org.seekloud.yubel.common._
import org.seekloud.yubel.ptcl._
import io.circe.generic.auto._
import org.seekloud.yubel.Boot.{executor, scheduler, timeout}
import org.seekloud.yubel.core.TokenActor
import org.seekloud.yubel.core.TokenActor.AskForToken
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.model.headers.{CacheDirective, `Cache-Control`}
import io.circe.Error
import org.seekloud.yubel.ptcl.EsheepPtcl._
import org.seekloud.utils.{CirceSupport, EsheepClient, SessionSupport}
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `public`}
import org.seekloud.yubel.http.SessionBase.{UserInfo, UserSession}
import org.seekloud.yubel.core.TokenActor.Command
import org.seekloud.utils.{CirceSupport, SessionSupport}

import scala.concurrent.Future
trait EsheepService extends ServiceUtils with CirceSupport with SessionSupport{

  private val log = LoggerFactory.getLogger(this.getClass)

  private val cacheSeconds = 24 * 60 * 60

  val tokenActor: akka.actor.typed.ActorRef[Command]

  private val playGame = (path("playGame") & get & pathEndOrSingleSlash) {
    parameter(
      'playerId.as[String],
      'playerName.as[String],
      'roomId.as[Int].?,
      'accessCode.as[String]
    ) {
      case (playerId, playerName, roomId, accessCode) =>
        val gameId = AppSettings.esheepGameId
        dealFutureResult{
          val msg: Future[String] = tokenActor ? AskForToken
          msg.map {token =>
              dealFutureResult{
                EsheepClient.verifyAccessCode(gameId, accessCode, token).map {
                  case Right(rsp) =>
                    if(rsp.playerId == playerId){
                      addCacheControlHeadersWithFilter(`public`, `max-age`(cacheSeconds)) {
                        getFromResource("html/index.html")
                      }
                    } else {
                      complete(ErrorRsp(120001, "Some errors happened in verifyAccessCode."))
                    }
                  case Left(e) =>
                    log.error(s"playGame error. fail to verifyAccessCode err: $e")
                    complete(ErrorRsp(120002, "Some errors happened in parse verifyAccessCode."))
                }
              }
          }
        }
    }
  }

  //fixme: for test
//  private val playGame = (path("playGame") & get & pathEndOrSingleSlash) {
//    parameter(
//      'playerId.as[String],
//      'playerName.as[String]
//    ) {
//      case (playerId, playerName) =>
//        getFromResource("html/index.html")
//    }
//  }

  private val watchGame = (path("watchGame") & get & pathEndOrSingleSlash) {
    log.info("success to render watchGame page.")
    getFromResource("html/index.html")
  }

  private val watchRecord = (path("watchRecord") & get & pathEndOrSingleSlash) {
    parameter(
      'recordId.as[Long],
      'playerId.as[String], //
      'frame.as[Int],
      'accessCode.as[String]
    ) {
      case (recordId, playerId, frame, accessCode) =>
        log.info("success to render watchRecord page.")
        getFromResource("html/index.html")
    }
  }

  private val getBotList = (path("getBotList") & post & pathEndOrSingleSlash) {
    entity(as[Either[Error,BotListReq]]) {
      case Right(v) =>
        val msg: Future[String] = tokenActor ? AskForToken
        dealFutureResult{
          msg.map{token =>
            dealFutureResult{
              EsheepClient.getBotList(v.userId, v.lastId, 10, token).map {
                case Right(r) =>
                  complete(r)
                case Left(e) =>
                  log.debug(s"Some errors happened in getBotList: $e")
                  complete(ErrorRsp(120003, "Some errors happened in getBotList."))
              }
            }
          }
        }
      case Left(e) =>
        log.debug(s"getBotList errs: $e")
        complete(ErrorRsp(120004, s"getBotList errs: $e"))
    }
  }

  //只使用强制缓存,去除协商缓存的字段
  def addCacheControlHeadersWithFilter(first: CacheDirective, more: CacheDirective*): Directive0 = {
    mapResponseHeaders { headers =>
      `Cache-Control`(first, more: _*) +: headers.filterNot(h => h.name() == "Last-Modified" || h.name() == "ETag")
    }
  }

  val esheepRoute: Route = playGame ~ watchRecord ~ watchGame ~ getBotList

}
