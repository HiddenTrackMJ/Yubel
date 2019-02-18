package org.seekloud.yubel.frontendAdmin

/**
  * User: Jason
  * Date: 2018/12/17
  * Time: 17:27
  */
object Routes {
  val baseUrl = "/yubel/admin"

  object Admin {
    val login = baseUrl + "/login"
    val logout = baseUrl + "/logout"
    val deleteUser = baseUrl + "/deleteUser"
    val updateUser = baseUrl + "/updateUser"
    val getAllUser = baseUrl + "/getAllUser"
    val getRoomList = baseUrl + "/getRoomList"
    val getRoomPlayerList = baseUrl + "/getRoomPlayerList"
    val getPlayerRecord = baseUrl + "/getPlayerRecord"
    val getPlayerRecordByTime = baseUrl + "/getPlayerRecordByTime"
    val getPlayerRecordAmount = baseUrl + "/getPlayerRecordAmount"
    val getPlayerByTimeAmount = baseUrl + "/getPlayerByTimeAmount"
  }
}
