package org.seekloud.utils

import org.seekloud.yubel.common.AppSettings
import org.seekloud.yubel.protocol.EsheepProtocol._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.seekloud.yubel.Boot.executor
//import org.seekloud.carnie.Boot.{scheduler, timeout, tokenActor}
import org.seekloud.yubel.core.TokenActor.AskForToken
import akka.actor.typed.scaladsl.AskPattern._
import org.seekloud.yubel.Boot
import org.seekloud.yubel.ptcl.EsheepPtcl.GetBotListRsp
import org.seekloud.yubel.ptcl.RoomApiProtocol.RoomListRsp4Client
import org.slf4j.LoggerFactory
import org.seekloud.yubel.ptcl._
import org.seekloud.utils.SecureUtil._

import scala.concurrent.Future
import scala.util.{Failure, Success}

object EsheepClient extends HttpUtil with CirceSupport {

  private val log = LoggerFactory.getLogger(this.getClass)

  val domain = AppSettings.esheepDomain
  private val baseUrl = AppSettings.esheepProtocol + "://" + domain + "/" + AppSettings.esheepUrl

  def getTokenRequest(gameId: Long, gsKey: String) = {

    val esheepUrl = baseUrl + "/api/gameServer/gsKey2Token"
    val sendData = GetToken(gameId, gsKey).asJson.noSpaces

    postJsonRequestSend(s"postUrl: $esheepUrl", esheepUrl, Nil, sendData).map {
      case Right(str) =>
        decode[GetTokenRsp](str) match {
          case Right(rsp) =>
            if(rsp.errCode==0) {
              Right(rsp.data)
            } else {
//              log.error(s"getTokenRequest error $esheepUrl rsp.error: ${rsp.msg}")
              Left("error")
            }
          case Left(err) =>
//            log.error(s"getTokenRequest error $esheepUrl parse.error $err")
            Left("error")
        }
      case Left(e) =>
//        log.error(s"getTokenRequest error $esheepUrl failed: $e")
        Left("error")
    }

  }

  def verifyAccessCode(gameId: Long, accessCode: String, token: String) = {
    val esheepUrl = baseUrl + s"/api/gameServer/verifyAccessCode?token=$token"
    val sendData = VerifyAccCode(gameId, accessCode).asJson.noSpaces

//    log.info("Start verifyAccessCode!")
    postJsonRequestSend(s"postUrl: $esheepUrl", esheepUrl, Nil, sendData).map {
      case Right(str) =>
        decode[VerifyAccCodeRsp](str) match {
          case Right(rsp) =>
            if(rsp.errCode==0){
              Right(rsp.data)
            } else {
//              log.error(s"verifyAccessCode error $esheepUrl rsp.error ${rsp.msg}")
              Left("error")
            }
          case Left(e) =>
//            log.error(s"verifyAccessCode error $esheepUrl parse.error $e")
            Left("error")
        }
      case Left(e) =>
//        log.error(s"verifyAccessCode error $esheepUrl failed: $e")
        Left("error")
    }
  }

  def inputBatRecord(
                      playerId: String,
                      nickname: String,
                      killing: Int,
                      killed: Int,
                      score: Float,
                      gameExtent: String = "",
                      startTime: Long,
                      endTime: Long,
                      token: String
                    ) = {
//    println("start inputBatRecord!")
    val gameId = AppSettings.esheepGameId
    val esheepUrl = baseUrl + s"/api/gameServer/addPlayerRecord?token=$token"
    val sendData = InputRecord(PlayerRecord(playerId, gameId, nickname, killing, killed, score, gameExtent, startTime, endTime)).asJson.noSpaces

    postJsonRequestSend(s"postUrl: $esheepUrl", esheepUrl, Nil, sendData).map {
      case Right(str) =>
        decode[ErrorRsp](str) match {
          case Right(rsp) =>
            if(rsp.errCode==0) {
              log.info(PlayerRecord(playerId, gameId, nickname, killing, killed, score, gameExtent, startTime, endTime).asJson.noSpaces)
//              println("finish inputBatRecord!")
              Right(rsp)
            } else {
//              log.error(s"inputBatRecord errorCode $esheepUrl rsp.error${rsp.msg}")
              Left("error.")
            }
          case Left(e) =>
//            log.error(s"inputBatRecord error $esheepUrl parse.error$e")
            Left("error.")
        }
      case Left(e) =>
//        log.error(s"inputBatRecord error $esheepUrl rsp.error$e")
        Left("error.")
    }
  }

  def getBotList(userId: Long,
                 lastId: Long,
                 count: Int,
                 token:String) = {
    val esheepUrl = baseUrl + s"/api/gameServer/getBotList?token=$token"
    val gameId = AppSettings.esheepGameId
    val sendData = GetBotListReq(gameId, userId, lastId, count).asJson.noSpaces

    postJsonRequestSend(s"postUrl: $esheepUrl", esheepUrl, Nil, sendData).map {
      case Right(str) =>
        decode[GetBotListRsp](str) match {
          case Right(rsp) =>
            if(rsp.errCode==0) {
//              println(s"getBotList in EsheepClient: $rsp")
              Right(rsp.data)
            } else {
//              log.error(s"getBotList error $esheepUrl rsp.error: ${rsp.msg}")
              Left("error")
            }
          case Left(err) =>
//            log.error(s"getBotList error $esheepUrl parse.error $err")
            Left("error")
        }
      case Left(e) =>
//        log.error(s"getBotList error $esheepUrl failed: $e")
        Left("error")
    }

  }

   def getRoomListInit() = {
    val url = s"http://$domain/carnie/getRoomList4Client"
    val appId = AppSettings.esheepGameId.toString
    val sn = appId + System.currentTimeMillis().toString
    val data = "".asJson.noSpaces
    val gsKey = AppSettings.esheepGsKey
    val (timestamp, nonce, signature) = generateSignatureParameters(List(appId, sn, data), gsKey)
    val params = PostEnvelope(appId, sn, timestamp, nonce, data,signature).asJson.noSpaces
    postJsonRequestSend("post",url,Nil,params).map{
      case Right(value) =>
        decode[RoomListRsp4Client](value) match {
          case Right(r) =>
            if(r.errCode == 0){
              println(s"roomData: $r")
              Right(r)
            }else{
              log.debug(s"获取列表失败，errCode:${r.errCode},msg:${r.msg}")
              Left("Error")
            }
          case Left(error) =>
            log.debug(s"获取房间列表失败1，${error}")
            Left("Error")

        }
      case Left(error) =>
        log.debug(s"获取房间列表失败2，${error}")
        Left("Error")
    }
  }




  def main(args: Array[String]): Unit = {

//    val appId = AppSettings.esheepGameId.toString
//    val sn = appId + System.currentTimeMillis().toString
//    val data = RoomApiProtocol.RecordByPlayerReq("test",0,5).asJson.noSpaces
//    val (timestamp, nonce, signature) = SecureUtil.generateSignatureParameters(List(appId, sn, data), AppSettings.esheepGsKey)
//    val params = PostEnvelope(appId, sn, timestamp, nonce, data,signature).asJson.noSpaces

//    val a = genPostEnvelope(AppSettings.esheepAppId,sn,data,AppSettings.esheepSecureKey)
    println(123)
//    val gsKey = AppSettings.esheepGsKey
//    val msg: Future[String] = tokenActor ? AskForToken
//    msg.map {token =>
//      println(s"token: $token")
//      verifyAccessCode(gameId, "1234456asdf", token)
//    }
    //    getTokenRequest(gameId, gsKey)
//    inputBatRecord("1", "asdtest", 1, 1, 10, "", 1L, 2L)
  }

}
