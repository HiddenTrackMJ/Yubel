package org.seekloud.carnie.frontendAdmin.pages

import org.seekloud.carnie.frontendAdmin.Routes
import org.seekloud.carnie.frontendAdmin.util.{Http, JsFunc, Page}
import org.seekloud.carnie.ptcl.AdminPtcl._
import org.seekloud.carnie.frontendAdmin.util.TimeTool
import mhtml.Var
import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.MouseEvent

import scala.xml.Elem
import scala.concurrent.ExecutionContext.Implicits.global
//import org.scalajs.dom
//import org.scalajs.dom.html.{Button, Input}
//import org.scalajs.dom.raw.KeyboardEvent
import io.circe.generic.auto._
import io.circe.syntax._
//
//import scala.xml.{Elem, Node}
/**
  * User: Jason
  * Date: 2018/12/18
  * Time: 13:04
  */
object ViewPage extends Page{
  override val locationHashString: String = "#/View"

  private val playerRecordsVar = Var(List.empty[PlayerRecord])

  private var playerAmount = 0

  private var playerAmountS = 0

  private var playerAmountToday= 0

  private var page = 1

  private val pageVar = Var(1,1)//left:page,right:pageNum

  private val pageLimit = 5

  val isGetAmount = Var(false)

  val isSearch = Var(false)

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
  val upBtn = isSearch.map{
    case false =>
      <button class="btn btn-default" onclick={(e:MouseEvent) => e.preventDefault();getPlayerRecord(page-1)} disabled={upBtnRx}>上一页</button>
    case true =>
      <button class="btn btn-default" onclick={(e:MouseEvent) => e.preventDefault();getPlayerRecordByTime(page-1)} disabled={upBtnRx}>上一页</button>
  }
  val downBtn = isSearch.map{
    case false =>
      <button class="btn btn-default" onclick={(e:MouseEvent) => e.preventDefault();getPlayerRecord(page+1)} disabled={downBtnRx}>下一页</button>
    case true =>
      <button class="btn btn-default" onclick={(e:MouseEvent) => e.preventDefault();getPlayerRecordByTime(page+1)} disabled={downBtnRx}>下 一页</button>
  }

  private val playerRecordsRx = playerRecordsVar.map{
    case Nil =>
      <div>
        <p style="text-align:center;">
          没有游戏记录
        </p>
      </div>
    case other =>
      showplayerRecords(other)
  }

  def showplayerRecords(list: List[PlayerRecord]) = {
    <div>
      {
      list.map {
        i =>
          showPlayerRecord(i)
      }
      }
      <div class="row" style="text-align:center;padding: 1rem 1rem 2rem 2rem;">
        {upBtn}{pageRx}/{pageNumRx}{downBtn}
      </div>
    </div>
  }

  private def showPlayerRecord(record: PlayerRecord) = {
    <div class="row" style="padding: 1rem 1rem 2rem 2rem;">
      <div class="col-xs-1" style="text-align:center;">
        {record.id}
      </div>
      <div class="col-xs-2" style="text-align:center;">
        {record.playerId}
      </div>
      <div class="col-xs-1" style="text-align:center;">
        {record.nickname}
      </div>
      <div class="col-xs-1" style="text-align:center;">
        {record.killing}
      </div>
      <div class="col-xs-1" style="text-align:center;">
        {record.killed}
      </div>
      <div class="col-xs-2" style="text-align:center;">
        {record.score}
      </div>
      <div class="col-xs-2" style="text-align:center;">
        {TimeTool.dateFormatDefault(record.startTime)}
      </div>
      <div class="col-xs-2" style="text-align:center;">
        {TimeTool.dateFormatDefault(record.endTime)}
      </div>
      <hr></hr>
      <br></br>
    </div>
  }


  def getPlayerRecord(page: Int) : Unit = {
    val url = Routes.Admin.getPlayerRecord
    val data = PageReq(page).asJson.noSpaces
    Http.postJsonAndParse[PlayerRecordRsp](url,data).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            playerRecordsVar := rsp.data
            ViewPage.page = page
            isSearch := false
            if(rsp.data.nonEmpty)
              pageVar := (page, rsp.playerAmount)
            else
              pageVar := (page, 0)

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

  def getPlayerRecordByTime(page: Int) : Unit = {
    val url = Routes.Admin.getPlayerRecordByTime
    val time = dom.document.getElementById("timeInput").asInstanceOf[Input].value.toString
    val data = PageTimeReq(page,time).asJson.noSpaces
    println("time: " + time)
    Http.postJsonAndParse[PlayerRecordRsp](url,data).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            playerRecordsVar := rsp.data
            ViewPage.page = page
            isSearch := true
            if(rsp.data.nonEmpty)
              pageVar := (page, rsp.playerAmount)
            else
              pageVar := (page, 0)
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

  def getPlayerRecordAmount() : Unit = {
    val url = Routes.Admin.getPlayerRecordAmount
    Http.getAndParse[PlayerAmountRsp](url).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            println(s"total :${rsp.playerAmount}, today: ${rsp.playerAmountToday}")
            playerAmount = rsp.playerAmount
            playerAmountToday = rsp.playerAmountToday
            isGetAmount := true
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

  def getPlayerByTimeAmount() : Unit = {
    val url = Routes.Admin.getPlayerByTimeAmount
    val time = dom.document.getElementById("timeInput").asInstanceOf[Input].value.toString
    val data = TimeReq(time).asJson.noSpaces
    println("time: " + time)
    Http.postJsonAndParse[PlayerByTimeAmountRsp](url,data).map{
      case Right(rsp) =>
        try {
          if (rsp.errCode == 0) {
            println(s"search :${rsp.playerAmount}")
            playerAmountS = rsp.playerAmount
            isGetAmount := true
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

  override def render: Elem = {
    getPlayerRecord(1)
    getPlayerRecordAmount()
    <div>
      <div class="container">
        <h1 style="text-align:center;font-family:楷体;">历史玩家记录</h1>
        <div>
          {
          isGetAmount.map{
            case true => <div style="font-family:楷体;font-size:20px;">历史玩家总数:{playerAmount},今日玩家数:{playerAmountToday},所选日期玩家总数:{playerAmountS}。</div>
            case _ => <div></div>
          }
          }
        </div>
        <div>
          <button class="btn btn-success" style="font-size:16px;border-radius:10px;outline:none" onclick={() => getPlayerRecordByTime(1);getPlayerByTimeAmount()}>按日期查询</button>
          <input  type="date" id="timeInput"></input>
        </div>
        <br></br>
        <h4 style="padding-left: 100px;"><b>游戏记录：</b></h4>
        <div class="row" style="padding: 1rem 1rem 2rem 2rem;">
          <div class="col-xs-1" style="text-align:center;">
            <label style="text-align:center">id:</label>
          </div>
          <div class="col-xs-2" style="text-align:center;">
            <label style="text-align:center">playerId:</label>
          </div>
          <div class="col-xs-1" style="text-align:center;">
            <label style="text-align:right">nickname:</label>
          </div>
          <div class="col-xs-1" style="text-align:center;">
            <label style="text-align:right">killing:</label>
          </div>
          <div class="col-xs-1" style="text-align:center;">
            <label style="text-align:right">killed:</label>
          </div>
          <div class="col-xs-2" style="text-align:center;">
            <label style="text-align:right">score:</label>
          </div>
          <div class="col-xs-2" style="text-align:center;overflow:auto;">
            <label style="text-align:right">startTime:</label>
          </div>
          <div class="col-xs-2" style="text-align:center;overflow:auto;">
            <label style="text-align:right">stopTime:</label>
          </div>
        </div>
        {playerRecordsRx}
      </div>
    </div>
  }
}
