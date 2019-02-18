package org.seekloud.yubel.paperClient

import org.seekloud.yubel.paperClient.Protocol.{frameRate1, frameRate2}
import org.seekloud.yubel.paperClient.WebSocketProtocol._
import org.seekloud.yubel.util.Component
import org.seekloud.yubel.{Main, Routes}
import org.seekloud.yubel.ptcl.RoomApiProtocol.RoomListRsp4Client
import org.seekloud.yubel.util.{Http, JsFunc}
import mhtml.{Rx, Var}
import io.circe.generic.auto._
import io.circe.syntax._
import org.scalajs.dom

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.util.Random
import scala.xml.Elem

class RoomListPage(webSocketPara: PlayGamePara) extends Component {

  case class Room(id:Int,model:String,isLocked:Boolean)
  var roomSelectMap : Map[Int,Boolean] =Map()
  var roomSelected = Room(-1,"",false)
  private val roomSelectFlag: Var[Map[Int, Boolean]] = Var(Map())
  val roomList: Var[List[Room]] = Var(List.empty[Room])
  var roomLists: List[Room] = List.empty[Room]
//  val roomLockMap:mutable.HashMap[Int, (Int, Boolean)] = mutable.HashMap.empty[Int, (Int, Boolean)]//(roomId -> (mode, hasPwd))
  var isReady = Var(false)

  def getRoomList() :Unit = {
    val url = Routes.yubel.getRoomList4Front
    Http.getAndParse[RoomListRsp4Client](url).map {
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            roomLists = rsp.data.roomList.sortBy(t => t).map { s =>
              val roomInfos = s.split("-")
              val roomId = roomInfos(0).toInt
              val modeName = if(roomInfos(1)=="0") "正常" else if(roomInfos(2)=="1") "反转" else "加速"
              val hasPwd = if(roomInfos(2)=="true") true else false
//              roomLockMap += roomId -> (roomInfos(1).toInt, hasPwd)
              Room(roomId , modeName ,hasPwd)
            }
            roomList := roomLists
            isReady := true
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
    }
  }

  def init() :Unit={
    roomLists.foreach(game=>
         roomSelectMap += (game.id -> false)
      )
    roomSelectFlag := roomSelectMap
    println(roomSelectFlag)
  }

  def selectClass(id: Int): Rx[String] = roomSelectFlag.map { flag =>
    if (flag.contains(id)) {
      flag(id) match {
        case true =>"game-selected"
        case _ =>"game-not-selected"
      }
    }
    else "game-not-selected"
  }

  def selectGame(id: Int): Unit = {
    if (roomSelectMap.contains(roomSelected.id))
      roomSelectMap += (roomSelected.id -> false)
    if (roomSelectMap.contains(id))
      roomSelectMap += (id -> true)
    roomSelectFlag := roomSelectMap
    roomSelected = roomLists.find {
      _.id == id
    }.get
    println(roomSelected.id,roomSelectFlag)
  }

  def gotoGame(roomId: Int, playerId: String, playerName: String): Unit = {
    val mod = roomSelected.model match {
      case "正常" => 0
      case "反转" => 1
      case "加速" => 2
    }
    if (roomId == -1) JsFunc.alert("请选择房间!")
    else {
      dom.document.getElementById("roomList").setAttribute("display","none")
      dom.document.getElementById("roomList").setAttribute("hidden","hidden")
      Main.refreshPage(new CanvasPage().render)
      new NetGameHolder("playGame", joinRoomByIdPara(roomId,playerId, playerName),mod).render
    }
  }

  private val renderRoomDiv = isReady.map{
    case true =>
      init()
      <div>
       {roomDiv}
      <div style="text-align: center;">
        <button type="button"   style="font-size: 30px ;" class="btn-primary" onclick=
        {() => gotoGame(roomSelected.id,webSocketPara.playerId,webSocketPara.playerName)}>进入游戏</button>
      </div>
      </div>
    case false =>
      <div style="text-align: center;">
        <h style="font-size: 35px;color:white;text-align: center;">获取房间出错！ </h>
      </div>
  }
  private val roomDiv = roomList.map{
    case Nil =>
      <div style="text-align: center;">
      <h style="font-size: 35px;color:white;text-align: center;">当前没有正在游戏的房间 </h>
      </div>
    case list =>
      <table style="width:1000 px;" align="center" >
        <thead class="thead-light" style="font-size: 20px;color:white;width:1000 px;">
          <tr>
            <th scope="col" style="width: 200px;">Locked Or Not</th>
            <th scope="col" style="width: 400px;">Room ID</th>
            <th scope="col" style="width: 400px;">Model</th>
          </tr>
        </thead>
        <tbody style="font-size: 20px;color:white;" >
          {list.map{l =>
          <tr class={selectClass(l.id)} onclick={()=>selectGame(l.id)} style="font-size: 20px;color:white;">
            <td>{if(l.isLocked) <img src={"/yubel/static/img/luffy.png"} style="width: 20px;height: 20px"></img>
                else <img  src={"/yubel/static/img/fatTiger.png"} style="width: 20px;height: 20px"></img>}</td>
            <td>{l.id}</td>
            <td>{l.model}</td>
          </tr>
            }
          }
        </tbody>
        </table>
  }

  override def render: Elem = {
    getRoomList()
    <div style="height:100%" id="roomList" >
      {renderRoomDiv}
    </div>
  }
}
