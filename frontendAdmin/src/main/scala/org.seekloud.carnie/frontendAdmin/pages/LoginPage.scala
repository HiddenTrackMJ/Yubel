package org.seekloud.carnie.frontendAdmin.pages

import org.seekloud.carnie.frontendAdmin.Routes
import org.seekloud.carnie.frontendAdmin.util.Page
import org.seekloud.carnie.frontendAdmin.util.{Http, JsFunc}
import org.seekloud.carnie.ptcl.AdminPtcl._
import mhtml.Var
import org.scalajs.dom
import org.scalajs.dom.html.{Button, Input}
import org.scalajs.dom.raw.KeyboardEvent
import io.circe.generic.auto._
import io.circe.syntax._

import scala.xml.{Elem, Node}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * User: Jason
  * Date: 2018/12/18
  * Time: 9:54
  */
object LoginPage extends Page{
  override val locationHashString: String = "#/LoginPage"

  def login():Unit = {
    val name = dom.window.document.getElementById("username").asInstanceOf[Input].value
    val password = dom.window.document.getElementById("password").asInstanceOf[Input].value
    val url = Routes.Admin.login
    val data = LoginReq(name, password).asJson.noSpaces
    Http.postJsonAndParse[SuccessRsp](url, data).map {
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            dom.window.location.href="#/CurrentDataPage"
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
        Yubel数据查看
      </div>
    </div>
  )

  val Btn:Var[Node]=Var(
    <div class="row" style="padding: 1rem 1rem 1rem 1rem;text-align:center;">
      <button id="logIn" class="btn btn-info" style="margin: 0rem 1rem 0rem 1rem;" onclick={()=>login() } >
        登陆
      </button>
    </div>
  )

  val Form:Var[Node]=Var(
    <form class="col-md-8 col-md-offset-2" style="border: 1px solid #dfdbdb;border-radius: 6px;padding:2rem 1rem 2rem 1rem;">
      {Email}
      {PassWord}
    </form>
  )

  override def render: Elem =
      <div>
        <div class="container">
          {Title}
          {Form}
        </div>
        <div class="container">
          {Btn}
        </div>
      </div>

}
