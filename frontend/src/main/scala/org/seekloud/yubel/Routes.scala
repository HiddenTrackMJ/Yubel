package org.seekloud.yubel

object Routes {
  val baseUrl = "/yubel"

  object Esheep {
    val playGame = baseUrl + "/esheep/playGame"
  }

  object yubel {
    val login = baseUrl + "/login"
    val signUp = baseUrl + "/signUp"
    val getRoomList = baseUrl + "/getRoomList"
    val updateRoomList = baseUrl + "/updateRoomList"
    val getRoomList4Front = baseUrl + "/getRoomList4Front"
    val addSession = baseUrl + "/addSession"
  }
}
