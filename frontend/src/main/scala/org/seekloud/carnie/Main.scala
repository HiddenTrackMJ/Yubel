package org.seekloud.carnie

import org.seekloud.carnie.paperClient.WebSocketProtocol._
import org.seekloud.carnie.paperClient._
import org.seekloud.carnie.paperClient.Protocol.frameRate1
import org.seekloud.carnie.ptcl.EsheepPtcl.PlayerMsg
import io.circe.generic.auto._
import io.circe.syntax._
import mhtml.{Cancelable, mount}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.Random
import scala.xml.Elem
/**
  * Created by haoshuhan on 2018/11/2.
  */

@JSExportTopLevel("paperClient.Main")
object Main extends js.JSApp {
  var currentPage:Elem = <div></div>
  def main(): Unit = {
    selectPage()
  }

  def newGameHolder(playerMsgMap: Map[String, String], info: Array[String]): Unit = {
    info(0) match {
      case "watchGame" =>
        val roomId = playerMsgMap.getOrElse("roomId", "1000")
        val playerId = playerMsgMap.getOrElse("playerId", "unknown")
        val accessCode = playerMsgMap.getOrElse("accessCode", "test123")
        println(s"Frontend-roomId: $roomId, playerId:$playerId, accessCode: $accessCode")
        new NetGameHolder4WatchGame("watchGame", WatchGamePara(roomId, playerId, accessCode)).init()

      case "watchRecord" =>
        val recordId = playerMsgMap.getOrElse("recordId", "1000001")
        val playerId = playerMsgMap.getOrElse("playerId", "1000001")
        val frame = playerMsgMap.getOrElse("frame", "0")
        val accessCode = playerMsgMap.getOrElse("accessCode", "abc")
        new NetGameHolder4WatchRecord(WatchRecordPara(recordId, playerId, frame, accessCode)).render

      case "playGame" =>
        println("playGame!")

      case _ =>
        println("Unknown order!")
    }
  }

  def selectPage():Unit = {
//    currentPage = new RoomListPage(PlayGamePara("test", "test")).render
    val r = Random.nextInt(1000)
    val headId = Random.nextInt(6)
    currentPage = new JoinGamePage("playGame", PlayGamePara(s"test$r", s"test$r")).render
    currentPage = new CanvasPage().render
    show()
    new NetGameHolder("playGame", PlayGamePara(s"娜可露露$r", s"娜可露露$r", 0, headId), 0, headId, frameRate1).init()
//    new NetGameHolder4WatchGame("watchGame", WatchGamePara(s"1000", s"bot_10001001", " ")).init()


//    val url = dom.window.location.href.split("carnie/")(1)
//    val info = url.split("\\?")
//    val playerMsgMap = info(1).split("&").map {
//      a =>
//        val b = a.split("=")
//        (b(0), b(1))
//    }.toMap
////    println(s"hello ${info(0)}....")
//    info(0) match {
//      case "playGame" =>
//        println("playGame ...")
//        val playerId = if (playerMsgMap.contains("playerId")) playerMsgMap("playerId") else "unKnown"
//        val playerName = if (playerMsgMap.contains("playerName")) playerMsgMap("playerName") else "unKnown"
////        currentPage = new JoinGamePage("playGame", PlayGamePara(playerId, playerName)).render
//        currentPage = new CanvasPage().render
//        show()
//        val modelId = 0
//        val frameRate = frameRate1
//        val headId = Random.nextInt(6)
//        new NetGameHolder("playGame", PlayGamePara(playerId, playerName, modelId, headId), modelId, headId, frameRate).init()
//
//      case _ =>
//        println(s"not playGame ${info(0)}")
//        currentPage = new CanvasPage().render
//        show()
//        newGameHolder(playerMsgMap, info)
//    }
  }

  def refreshPage(newPage: Elem): Cancelable = {
    println("refreshPage!!!")
//    dom.document.body.removeChild(dom.document.body.firstChild)
    currentPage = newPage
    show()
  }

  def show(): Cancelable = {
    mount(dom.document.body, currentPage)
  }

//  def play(modelId:Int, headId:Int,playerId:String, playerName:String): Unit = {
//    currentPage = new CanvasPage().render
//    val page =
//      <div>
//        {currentPage}
//      </div>
//    mount(dom.document.body, page)
//    val url = dom.window.location.href.split("carnie/")(1)
//    val info = url.split("\\?")
//    val playerMsgMap = info(1).split("&").map {
//      a =>
//        val b = a.split("=")
//        (b(0), b(1))
//    }.toMap
//    val sendData = PlayerMsg(playerMsgMap).asJson.noSpaces
//    println(s"sendData: $sendData")
//    val playerId = if (playerMsgMap.contains("playerId")) playerMsgMap("playerId") else "unKnown"
//    val playerName = if (playerMsgMap.contains("playerName")) playerMsgMap("playerName") else "unKnown"
//    new NetGameHolder("playGame", PlayGamePara(playerId, playerName,modelId,headId)).init()
//    currentPage = new NetGameHolder("playGame", PlayGamePara("test", "test",modelId,headId)).render
//  }
}
