package org.seekloud.yubel

import org.seekloud.yubel.paperClient.WebSocketProtocol._
import org.seekloud.yubel.paperClient._
import org.seekloud.yubel.paperClient.Protocol.frameRate1
import org.seekloud.yubel.ptcl.EsheepPtcl.PlayerMsg
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


  def selectPage():Unit = {
//    currentPage = new RoomListPage(PlayGamePara("test", "test")).render
//    val r = Random.nextInt(1000)
//    val headId = Random.nextInt(6)
//    currentPage = new CanvasPage().render

//    new NetGameHolder("playGame", PlayGamePara(s"test$r", s"test$r", 0, headId), 0, headId, frameRate1).init()
    currentPage = new JoinPage().render
    show()
    //    new NetGameHolder4WatchGame("watchGame", WatchGamePara(s"1000", s"bot_10001001", " ")).init()


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

}
