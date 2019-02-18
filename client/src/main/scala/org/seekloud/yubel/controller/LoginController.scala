package org.seekloud.yubel.controller

import java.io.ByteArrayInputStream

import akka.actor.typed.ActorRef
import org.seekloud.yubel.Boot
import org.seekloud.yubel.actor.LoginSocketClient
import org.seekloud.yubel.actor.LoginSocketClient.{Connection2EsByMail, EstablishConnection2Es}
import org.seekloud.yubel.scene._
import org.seekloud.yubel.common.Context
import org.seekloud.yubel.utils.Api4GameAgent._
import org.seekloud.yubel.Boot.{executor, materializer, system}
import akka.actor.typed.scaladsl.adapter._
import org.seekloud.yubel.paperClient.ClientProtocol.PlayerInfoInClient
import org.seekloud.yubel.utils.{Api4GameAgent, WarningDialog}
import javafx.geometry.Insets
import javafx.scene.control.ButtonBar.ButtonData
import javafx.scene.control._
import javafx.scene.layout.GridPane
import javafx.scene.text.{Font, FontWeight}
import org.slf4j.LoggerFactory

/**
  * Created by dry on 2018/10/26.
  **/

class LoginController(modeScene: ModeScene, loginScene: LoginScene, context: Context) {//mode: Int, img: Int

  val loginSocketClient = system.spawn(LoginSocketClient.create(context, this), "loginSocketClient")

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  loginScene.setLoginSceneListener(new LoginScene.LoginSceneListener {
    override def loginByMail(): Unit = {
      Boot.addToPlatform{
        val rst = initLoginDialog()
        if(rst.nonEmpty) {
          Api4GameAgent.loginByMail(rst.get._1, rst.get._2).map {
            case Right(r) =>
              log.info(s"got loginInfo: $r")
              loginSocketClient ! Connection2EsByMail(r.userId, r.userName, r.token)
            case Left(_) =>
              log.debug("failed to getLoginRspFromEs.")
              Boot.addToPlatform(
                WarningDialog.initWarningDialog("邮箱账号或密码错误！")
              )
//              loginSocketClient ! Connection2EsByMail(10000, "test123", "test123")
          }
        }
      }
    }

    override def comeBack(): Unit = {
      Boot.addToPlatform{
        Boot.system.stop(loginSocketClient.toUntyped)
        context.switchScene(modeScene.getScene, "模式选择", false)
      }
    }
  })

  def initLoginDialog() = {
    val dialog = new Dialog[(String,String)]()
    dialog.setTitle("登录窗口")
    val nameField = new TextField()
    val pwdField = new PasswordField()
    val confirmButton = new ButtonType("确定", ButtonData.OK_DONE)
    val grid = new GridPane
    grid.setHgap(10)
    grid.setVgap(10)
    grid.setPadding(new Insets(10,10,15,10))
    grid.add(new Label("username:"), 0 ,0)
    grid.add(nameField, 1 ,0)
    grid.add(new Label("password:"), 0 ,1)
    grid.add(pwdField, 1 ,1)
    dialog.getDialogPane.getButtonTypes.addAll(confirmButton, ButtonType.CANCEL)
    dialog.getDialogPane.setContent(grid)
    dialog.setResultConverter(dialogButton =>
      if(dialogButton == confirmButton)
        (nameField.getText(), pwdField.getText())
      else
        null
    )
    var loginInfo:Option[(String,String)] = None
    val rst = dialog.showAndWait()
    rst.ifPresent { a =>
      if(a._1!=null && a._2!=null && a._1!="" && a._2!="")
        loginInfo = Some((a._1,a._2))
      else
        None
    }
    loginInfo
  }

  def init(): Unit = {
    Boot.addToPlatform(
      getLoginRspFromEs().map {
        case Right(r) =>
          val wsUrl = r.wsUrl
          val scanUrl = r.scanUrl
          loginScene.drawScanUrl(imageFromBase64(scanUrl))
          loginSocketClient ! EstablishConnection2Es(wsUrl)

        case Left(e) =>
          log.debug(s"failed to getLoginRspFromEs: $e")
      }
    )
  }

  def imageFromBase64(base64Str:String): ByteArrayInputStream  = {
    if(base64Str == null) null

    import java.util.Base64
//    import sun.misc.BASE64Decoder
    val decoder = Base64.getDecoder
    val bytes:Array[Byte]= decoder.decode(base64Str)
    bytes.indices.foreach{ i =>
      if(bytes(i) < 0) bytes(i)=(bytes(i)+256).toByte
    }
    val  b = new ByteArrayInputStream(bytes)
    b
  }

  def showScene() {
    Boot.addToPlatform {
      context.switchScene(loginScene.scene, "Login", false)
    }
  }

  def switchToSelecting(playerInfoInClient: PlayerInfoInClient):Unit = {
    Boot.addToPlatform {
      val selectScene = new SelectScene()
      new SelectController(playerInfoInClient, selectScene, context).showScene
    }
  }

//  def switchToRoomList(playerInfoInClient: PlayerInfoInClient, domain: String):Unit = {
//    Boot.addToPlatform {
//      val roomListScene = new RoomListScene()
//      new RoomListController(playerInfoInClient, roomListScene, context, domain).showScene
//    }
//  }
}
