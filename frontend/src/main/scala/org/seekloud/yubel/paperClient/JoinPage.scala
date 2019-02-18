package org.seekloud.yubel.paperClient


import org.seekloud.yubel.util.{Component, Http, JsFunc, Modal}
import org.seekloud.yubel.paperClient.Protocol.frameRate1
import org.seekloud.yubel.paperClient.WebSocketProtocol.{CreateRoomPara, PlayGamePara, WebSocketPara}
import org.scalajs.dom
import org.scalajs.dom.html.{Button, Input}
import org.scalajs.dom.raw.KeyboardEvent
import org.seekloud.yubel.Routes
import org.seekloud.yubel.ptcl.UserPtcl._

import scala.util.Random
import scala.xml.Node
import scala.concurrent.ExecutionContext.Implicits.global
import org.seekloud.yubel.Main
import mhtml.{Rx, Var}

import io.circe.generic.auto._
import io.circe.syntax._

import scala.xml.Elem

/**
  * User: Jason
  * Date: 2019/2/17
  * Time: 14:00
  */
class JoinPage extends Component {

  def login():Unit = {
    val name = dom.window.document.getElementById("username").asInstanceOf[Input].value
    val password = dom.window.document.getElementById("password").asInstanceOf[Input].value
    val url = Routes.yubel.login
    val data = LoginReq(name, password).asJson.noSpaces
    Http.postJsonAndParse[IdRsp](url, data).map {
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            gotoGame(0,1,rsp.id.toString,name)
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
        JsFunc.alert("Login error!")
    }
  }

  def signUp(): Unit = {
    val name = dom.window.document.getElementById("name").asInstanceOf[Input].value
    val password = dom.window.document.getElementById("pwd").asInstanceOf[Input].value
    val url = Routes.yubel.signUp
    val data = AddUserReq(name, password).asJson.noSpaces
    Http.postJsonAndParse[SuccessRsp](url, data).map {
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            JsFunc.alert("Create new account successfully!")
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
        JsFunc.alert("Login error!")
    }
  }

  def gotoGame(modelId: Int, headId: Int, playerId: String, playerName: String): Unit = {
    dom.document.getElementById("all").setAttribute("display","none")
    dom.document.getElementById("all").setAttribute("hidden","hidden")
    Main.refreshPage(new CanvasPage().render)
    dom.document.getElementById("all").setAttribute("display","none")
    dom.document.getElementById("all").setAttribute("hidden","hidden")
    new NetGameHolder("playGame", PlayGamePara(playerId, playerName, modelId, headId), modelId, headId, frameRate1).init()
  }

  val Email:Var[Node] =Var(
    <div class="row" style="padding: 1rem 1rem 1rem 1rem;">
      <label class="col-md-3" style="text-align:right">用户名</label>
      <div class="col-md-6">
        <input type="text" id="username" placeholder="用户名" class="form-control" autofocus="true"></input>
      </div>
    </div>
  )

  val PassWord:Var[Node] =Var(
    <div class="row" style="padding: 1rem 1rem 1rem 1rem">
      <label class="col-md-3" style="text-align:right;">密码</label>
      <div class="col-md-6">
        <input type="password" id="password" placeholder="密码" class="form-control" onkeydown={e:KeyboardEvent => loginByEnter(e)}></input>
      </div>
    </div>
  )

  def loginByEnter(event: KeyboardEvent):Unit = {
    if(event.keyCode == 13)
      dom.document.getElementById("logIn").asInstanceOf[Button].click()
  }

  val Title:Var[Node]=Var(
    <div class="row" style="margin-top: 15rem;margin-bottom: 4rem;">
      <div style="text-align: center;font-size: 4rem;">
        Yubel用户登录
      </div>
    </div>
  )

  def makeModal:Elem = {
    val title = <h4 class="modal-title" style="text-align: center;">注册</h4>
    val child =
      <div>
        <form style="border: 1px solid #dfdbdb;border-radius: 3px;padding:2rem 1rem 2rem 1rem;">
          <div class="row" style="padding: 1rem 1rem 1rem 1rem;">
            <label class="col-md-3" style="text-align:right">用户名</label>
            <div class="col-md-6">
              <input type="text" id="name" placeholder="用户名" class="form-control" autofocus="true"></input>
            </div>
          </div>
          <div class="row" style="padding: 1rem 1rem 1rem 1rem">
            <label class="col-md-3" style="text-align:right;">密码</label>
            <div class="col-md-6">
              <input type="password" id="pwd" placeholder="密码" class="form-control" onkeydown={e:KeyboardEvent => loginByEnter(e)}></input>
            </div>
          </div>
        </form>
      </div>
    new Modal(title, child, () => signUp(), s"signUp").render
  }

  val Btn:Var[Node]=Var(
    <div class="row" style="padding: 1rem 1rem 1rem 1rem;text-align:center;">
      <button id="logIn" class="btn btn-info" style="margin: 0rem 1rem 0rem 1rem;"
              onclick={()=> login()} >
        登陆
      </button>
      <button id="logIn" class="btn btn-info" style="margin: 0rem 1rem 0rem 1rem;"
              onclick={()=>
                val rnd = new Random()
                val id = rnd.nextInt(1000000)
                gotoGame(0,1,s"游客${id}号",s"游客${id}号")} >
        匿名登陆
      </button>
      <button id="random" class="btn btn-info" style="margin: 0rem 1rem 0rem 1rem;"
              data-toggle="modal" data-target={s"#signUp"} onclick={() => println(111)}>注册</button>
      <div>{makeModal}</div>

    </div>
  )

  val Form:Var[Node]=Var(
    <form class="col-md-8 col-md-offset-2" style="border: 1px solid #dfdbdb;border-radius: 6px;padding:2rem 1rem 2rem 1rem;">
      {Email}
      {PassWord}
    </form>
  )

  override def render: Elem =
    <div id="all">
      <div class="container">
        {Title}
        {Form}
      </div>
      <div class="container">
        {Btn}
      </div>
    </div>

}
