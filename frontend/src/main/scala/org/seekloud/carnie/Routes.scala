package org.seekloud.carnie

object Routes {
  val baseUrl = "/carnie"

  object Esheep {
    val playGame = baseUrl + "/esheep/playGame"
  }

  object Carnie {
    val login = baseUrl + "/login"
    val signUp = baseUrl + "/signUp"
    val getRoomList = baseUrl + "/getRoomList"
    val updateRoomList = baseUrl + "/updateRoomList"
    val getRoomList4Front = baseUrl + "/getRoomList4Front"
    val addSession = baseUrl + "/addSession"
  }
}
