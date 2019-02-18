package org.seekloud.yubel.paperClient

import org.seekloud.yubel.common.Constant
import org.seekloud.yubel.util.JsFunc
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.yubel.paperClient.WebSocketProtocol.{CreateRoomPara, PlayGamePara, WebSocketPara}
import org.seekloud.yubel.util.Component
import org.scalajs.dom
import org.scalajs.dom.html.Input

import scala.util.Random
import org.scalajs.dom.raw.{Event, HTMLAudioElement, VisibilityState}
import org.seekloud.yubel.Main
import mhtml.{Rx, Var}
import scala.scalajs.js.Date
import io.circe.generic.auto._
import io.circe.syntax._


import scala.xml.Elem

class CreateRoomPage(order: String, webSocketPara: PlayGamePara) extends Component {

  sealed case class Model(id: Int, img: String, name: String)

  sealed case class Head(id: Int, img: String)

  private var windowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)

  var modelLists = List(Model(0,"/yubel/static/img/coffee2.png","正常模式"),
    Model(1,"/yubel/static/img/game.png","反转模式"),Model(2,"/yubel/static/img/rocket1.png","加速模式"))
  var modelSelectMap : Map[Int,Boolean] =Map()
  var modelSelected = Model(0,"/yubel/static/img/coffee2.png","正常模式")
  //模式选择框
  private val modelList: Var[List[Model]] = Var(modelLists)
  private val modelSelectFlag: Var[Map[Int, Boolean]] = Var(Map())

  var headLists = List(Head(0, "/yubel/static/img/luffy.png"), Head(1, "/yubel/static/img/fatTiger.png"), Head(2, "/yubel/static/img/Bob.png"),
    Head(3, "/yubel/static/img/yang.png"), Head(4, "/yubel/static/img/smile.png"), Head(5, "/yubel/static/img/pig.png"))
  var headSelectMap: Map[Int, Boolean] = Map()
  var headSelected = Head(0, "/yubel/static/img/luffy.png")
  //头像选择框
  private val headList: Var[List[Head]] = Var(headLists)
  private val headSelectFlag: Var[Map[Int, Boolean]] = Var(Map())

  private val modelDiv = modelList.map { games =>
    games.map(game =>
      <div style="width:27%;margin:10px;">
        <div style="overflow:hidden" id={game.id.toString}>
          <div class={selectClass(game.id)} onclick={()=>selectGame(game.id)} style="margin-top:0px;height:150px;width:150px;">
            <img class="home-img" src={game.img}></img>
          </div>
          <p style="font-size: 15px;color:white;margin-left:15%;margin-right:15%" > {game.name}</p>
        </div>
      </div>
    )
  }

  private val headDiv = headList.map { games =>
    games.map(game =>
      <div style="width:27%;margin:15px;">
        <div style="overflow:hidden" id={game.id.toString}>
          <div class={selectHeadClass(game.id)} onclick={() => selectHead(game.id)} style="margin-top:10px;height:100px;width:100px;text-align: center">
            <img class="home-img" src={game.img}></img>
          </div>
        </div>
      </div>
    )
  }
  def init()={
    modelLists.foreach(game=>
      if(game.id == 0) modelSelectMap += (game.id -> true)
      else modelSelectMap += (game.id -> false)
    )
    modelSelectFlag := modelSelectMap
    val rnd = Random.nextInt(6)
    headLists.foreach(game =>
      if(game.id == rnd) headSelectMap += (game.id -> true)
      else headSelectMap += (game.id -> false)
    )
    headSelectFlag := headSelectMap
    headSelected = headLists(rnd)
  }

  def selectClass(id: Int): Rx[String] = modelSelectFlag.map { flag =>
    if (flag.contains(id)) {
      flag(id) match {
        case true =>"game-selected"
        case _ =>"game-not-selected"
      }
    }
    else "game-not-selected"
  }

  def selectGame(id: Int): Unit = {
    if (modelSelectMap.contains(modelSelected.id))
      modelSelectMap += (modelSelected.id -> false)
    if (modelSelectMap.contains(id))
      modelSelectMap += (id -> true)
    modelSelectFlag := modelSelectMap
    modelSelected = modelLists.find {
      _.id == id
    }.get
    println(modelSelected.name)
  }

  def selectHeadClass(id: Int): Rx[String] = headSelectFlag.map { flag =>
    if (flag.contains(id)) {
      flag(id) match {
        case true => "game-selected"
        case _ => "game-not-selected"
      }
    }
    else "game-not-selected"
  }

  def selectHead(id: Int): Unit = {
    if (headSelectMap.contains(headSelected.id))
      headSelectMap += (headSelected.id -> false)
    if (headSelectMap.contains(id))
      headSelectMap += (id -> true)
    headSelectFlag := headSelectMap
    headSelected = headLists.find {
      _.id == id
    }.get
    println(headSelected.id)
  }

  def resetScreen(): Unit = {
    if(dom.window.innerWidth.toFloat > windowBoundary.x)
      windowBoundary = Point(dom.window.innerWidth.toFloat, dom.window.innerHeight.toFloat)
  }
  def gotoGame(modelId: Int, headId: Int, playerId: String, playerName: String): Unit = {
    if (modelId == -1 || headId == -1) JsFunc.alert("请选择模式和头像!")
    else {
      val pwd = dom.document.getElementById("pwdInput").asInstanceOf[Input].value
      println(s"pwd: $pwd")
      Main.refreshPage(new CanvasPage().render)
      val frameRate = if(modelId==2) frameRate2 else frameRate1
      new NetGameHolder("playGame", CreateRoomPara(playerId, playerName, pwd, modelId, headId), modelId, headId, frameRate).init()
    }
  }
  override def render: Elem = {
    {init()}
    <div id ="resizeDiv">
      <div  style="background-color: #333333;height:750px" id="body" >
        <div  id="selectPage">
          <div  id="form">
            <h1 style="font-family: Verdana;font-size:30px;color:white;text-align: center;" >欢迎来到yubel</h1>
          </div>
          <div style="overflow: hidden;" >
            <div style="display:flex;flex-direction: row;flex-wrap: wrap;justify-content: center;align-items:center;margin-left:20%;margin-right:20%;text-align:center" >
              {modelDiv}
            </div>
          </div>

          <div style="text-align: center;">
            <button type="button"   style="font-size: 30px ;" class="btn-primary" onclick=
            {() => gotoGame(modelSelected.id,headSelected.id,webSocketPara.playerId,webSocketPara.playerName)}>创建房间</button>
          </div>

          <dvi style="text-align: center;margin-left:46%;">
            <input id="pwdInput" style="width:100px;" type="password">password: </input>
          </dvi>

          <div style="overflow: hidden;" >
            <div style="margin-top: 10px;">
              <p style="text-align: center; margin-top: 0px;font-size: 20px;color:white" >选择头像</p>
            </div>
            <div  style="text-align: center;display: flex; flex-wrap: nowrap;margin-left:12%;margin-right:12%">
              {headDiv}
            </div>
          </div>
        </div>
      </div>
    </div>
  }


}
