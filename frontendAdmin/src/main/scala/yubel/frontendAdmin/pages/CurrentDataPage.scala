package org.seekloud.yubel.frontendAdmin.pages

import org.seekloud.yubel.frontendAdmin.Routes
import org.seekloud.yubel.frontendAdmin.util.{Http, JsFunc, Page}
import org.seekloud.yubel.ptcl.AdminPtcl._
import org.seekloud.yubel.ptcl.RoomApiProtocol._
import mhtml.Var
import org.scalajs.dom
import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.mutable
import scala.util.{Failure, Success}
import scala.xml.{Elem, Node}
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * User: Jason
  * Date: 2018/12/19
  * Time: 9:57
  */
object CurrentDataPage extends Page{
  override val locationHashString: String = "#/CurrentDataPage"

  case class Room(id: Int, model: String)

  val roomListVar: Var[List[Room]] = Var(List.empty)

  var roomList: List[Room] = List.empty

  val roomPlayerList: Var[List[PlayerIdName]] = Var(List.empty)

//  var roomPlayerList: List[PlayerIdName] = List.empty

//  var roomPlayerMap: Map[Room,List[PlayerIdName]] = Map()

  var roomMap: Map[Int, (Int, Option[String], mutable.HashSet[(String, String)])] = Map()

  var roomPlayerMap: Map[Int, mutable.HashSet[(String, String)]] = Map()


  def getRoomPlayerList():Unit ={
    val url = Routes.Admin.getRoomPlayerList
//    val data = RoomIdReq(roomId).asJson.noSpaces
    Http.getAndParse[RoomMapRsp](url).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            roomMap = rsp.data.roomMap.toMap
            roomPlayerMap = roomMap.map(i => i._1 -> i._2._3)
            roomList = roomMap.map {
              i =>
                val roomId = i._1
                val modeName = if (i._2._1 == 0) "正常" else if (i._2._1 == 1) "反转" else "加速"
                Room(roomId, modeName)
            }.toList
            roomListVar := roomList
          }
          else {
            println("error======" + rsp.msg)
            JsFunc.alert(rsp.msg)
          }
        }
        catch {
          case e: Exception =>
            println(e)
        }

      case Left(e) =>
        println("error======" + e)
        JsFunc.alert("对不起，您尚未登陆!")
        dom.window.location.href=""
    }
  }

  private val roomDiv = roomListVar.map{
    case Nil =>
      <div style="text-align: center;"><h3>暂无数据</h3></div>

    case r => showRoomL(r)

  }

  private def showRoomL(roomL: List[Room]):Elem = {
    <div>
      <div style="margin-top: 30px;">
        <div class="row">
          <div class="col-xs-2">
          </div>
          <div class="col-xs-1" style="text-align:center;font-weight:bold">roomId</div>
          <div class="col-xs-1" style="text-align:center;font-weight:bold">model</div>
          <div class="col-xs-2" style="text-align:center;font-weight:bold">amount of players</div>
          <div class="col-xs-2" style="text-align:left;font-weight:bold">playerId</div>
          <div class="col-xs-2" style="text-align:left;font-weight:bold">playerName</div>
          <br></br>
        </div>
        <div>
          {roomL.map { i => showRoom(i) }}
        </div>
      </div>
    </div>
  }

  private def showRoom(room: Room) = {
    <div>
      <div class="row">
        <div class="col-xs-2">
        </div>
        <div class="col-xs-1" style="text-align:center;">
          {room.id}
        </div>
        <div class="col-xs-1" style="text-align:center;">
          {room.model}
        </div>
        <div class="col-xs-2" style="text-align:center;">
          {roomPlayerMap(room.id).toList.length}
        </div>
        <div class="col-xs-2" style="text-align:left;">
          {
          var n = 0
          roomPlayerMap(room.id).toList.map{
            i =>
              n += 1
              <div>{s"$n、${i._1}"}</div>
          }
          }
        </div>
        <div class="col-xs-2" style="text-align:left;">
          {
          roomPlayerMap(room.id).toList.map{
            i =>
              <div>{s"${i._2}"}</div>
          }
          }
        </div>
      </div>
      <hr />
      <br></br>
    </div>

  }



  override def render: Elem = {
    getRoomPlayerList()
//    getRoomList()
    <div>
      {roomDiv}
    </div>
  }
}
