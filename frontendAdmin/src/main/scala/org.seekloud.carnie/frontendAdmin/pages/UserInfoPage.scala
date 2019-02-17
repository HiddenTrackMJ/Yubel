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

import scala.xml.Elem

/**
  * User: Jason
  * Date: 2019/2/17
  * Time: 19:14
  */
object UserInfoPage extends Page{
  override val locationHashString: String = "#/UserInfoPage"

  val allUsers = Var(List.empty[UserInfo])

  def getAllUser(): Unit = {
    val url = Routes.Admin.getAllUser
    Http.getAndParse[AllUserRsp](url).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            allUsers := rsp.users
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

  override def render: Elem =
    <div>
    </div>
}
