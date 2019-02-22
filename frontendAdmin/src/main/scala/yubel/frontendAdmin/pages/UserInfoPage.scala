package org.seekloud.yubel.frontendAdmin.pages

import org.seekloud.yubel.frontendAdmin.Routes
import org.seekloud.yubel.frontendAdmin.util.{Http, JsFunc, Page, TimeTool}
import org.seekloud.yubel.ptcl.AdminPtcl._
import mhtml.Var
import org.scalajs.dom
import org.scalajs.dom.html.{Button, Input}
import org.scalajs.dom.raw.{KeyboardEvent, MouseEvent}
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

  private var allUsers = List.empty[UserInfo]

  private val partUsers =  Var(List.empty[UserInfo])

  private var userAmount = 0

  private var page = 1

  private val pageVar = Var(1,1)//left:page,right:pageNum

  private val pageLimit = 5

  val isGetAmount = Var(false)

  val pageRx = pageVar.map{
    i => i._1
  }

  val pageNumRx = pageVar.map{
    i =>
      if(i._2%pageLimit == 0)
        i._2/pageLimit
      else
        i._2/pageLimit + 1
  }

  val upBtnRx = pageVar.map {
    i =>
      if(i._1>1)
        None
      else
        Some("disabled")
  }

  val downBtnRx = pageVar.map {
    i =>
      if(i._1*pageLimit<i._2)
        None
      else
        Some("disabled")
  }
  val upBtn = {
      <button class="btn btn-default" onclick={(e:MouseEvent) => e.preventDefault();getUserByPage(page-1)} disabled={upBtnRx}>上一页</button>
  }
  val downBtn = {
      <button class="btn btn-default" onclick={(e:MouseEvent) => e.preventDefault();getUserByPage(page+1)} disabled={downBtnRx}>下 一页</button>
  }

  def getUserByPage(page: Int) = {
    partUsers := allUsers.slice((page - 1) * 5, page * 5)
    this.page = page
  }

  private val partUsersRx = partUsers.map{
    case Nil =>
      <div>
        <p style="text-align:center;">
          没有用户记录
        </p>
      </div>
    case other =>
      showallUsers(other)
  }

  def showallUsers(list: List[UserInfo]) = {
    <div>
      {
      list.map {
        i =>
          showUser(i)
      }
      }
      <div class="row" style="text-align:center;padding: 1rem 1rem 2rem 2rem;">
        {upBtn}{pageRx}/{pageNumRx}{downBtn}
      </div>
    </div>
  }

  private def showUser(record: UserInfo) = {
    <div class="row" style="padding: 1rem 1rem 2rem 2rem;">
      <div class="col-xs-1" style="text-align:center;">
        {record.id}
      </div>
      <div class="col-xs-2" style="text-align:center;" placeHolder={s"${record.name}"} id={s"name-${record.id}"}>
        {record.name}
      </div>
      <input class="col-xs-2" style="text-align:center;" placeHolder={s"${record.pwd}"} id={s"pwd-${record.id}"}>
        {record.pwd}
      </input>
      <div class="col-xs-2" style="text-align:center;" >
        {TimeTool.dateFormatDefault(record.createTime)}
      </div>
      <input class="col-xs-1" style="text-align:center;" placeHolder={s"${record.state}"} id={s"state-${record.id}"}>
        {record.state}
      </input>
      <button class="col-xs-1" style="text-align:center;" onclick={() =>
        updateUser(record.id,record.name,record.pwd,record.state) }>
        修改
      </button>
      <hr></hr>
      <br></br>
    </div>
  }

  def getAllUser(): Unit = {
    val url = Routes.Admin.getAllUser
    Http.getAndParse[AllUserRsp](url).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            allUsers = rsp.users
            getUserByPage(1)
            userAmount = rsp.users.length
            isGetAmount :=  true
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
        JsFunc.alert("get all user error!")
    }
  }

  def updateUser(id: Long,name: String,pwd: String,state: Int): Unit = {
    val url = Routes.Admin.updateUser
    val n = dom.document.getElementById(s"name-$id").asInstanceOf[Input].value.toString
    val p = dom.document.getElementById(s"pwd-$id").asInstanceOf[Input].value.toString
    val s = dom.document.getElementById(s"state-$id").asInstanceOf[Input].value
    val stateX = if (s.toString != "") s.toInt else state
    val nameX = if (n != "") n else name
    val pwdX = if (p != "") p else pwd
    val data = UpdateUserReq(nameX,pwdX,stateX).asJson.noSpaces
    println(data)
    Http.postJsonAndParse[SuccessRsp](url,data).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            JsFunc.alert("Update Successfully!")
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
        JsFunc.alert("update user error!")
    }
  }




  override def render: Elem = {
    getAllUser()
    <div>
      <div class="container">
        <h1 style="text-align:center;font-family:楷体;">历史用户信息</h1>
        <div>
          {
          isGetAmount.map{
            case true => <div style="font-family:楷体;font-size:20px;">用户总数:{userAmount}</div>
            case _ => <div></div>
          }
          }
        </div>
        <br></br>
        <h4 style="padding-left: 100px;"><b>用户信息：</b></h4>
        <div class="row" style="padding: 1rem 1rem 2rem 2rem;">
          <div class="col-xs-1" style="text-align:center;">
            <label style="text-align:center">id:</label>
          </div>
          <div class="col-xs-2" style="text-align:center;">
            <label style="text-align:center">name:</label>
          </div>
          <div class="col-xs-2" style="text-align:center;">
            <label style="text-align:right">pwd:</label>
          </div>
          <div class="col-xs-2" style="text-align:center;">
            <label style="text-align:right">createTime:</label>
          </div>
          <div class="col-xs-1" style="text-align:center;">
            <label style="text-align:right">state:</label>
          </div>
          <div class="col-xs-1" style="text-align:center;">
            <label style="text-align:right">action:</label>
          </div>
        </div>
        {partUsersRx}
      </div>
    </div>
  }
}
