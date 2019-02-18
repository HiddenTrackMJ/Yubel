package org.seekloud.yubel.paperClient

/**
  * Created by dry on 2018/10/31.
  **/
object ClientProtocol {

  case class PlayerInfoInClient(id: String, name: String, token: String = "", accessCode: String = "")//msg:token or accessCode
}
