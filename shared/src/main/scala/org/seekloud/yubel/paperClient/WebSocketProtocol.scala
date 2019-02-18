package org.seekloud.yubel.paperClient

/**
  * Created by haoshuhan on 2018/11/2.
  */
object WebSocketProtocol {
  sealed trait WebSocketPara

  case class PlayGamePara(playerId: String, playerName: String, mode: Int = 0, img: Int = 0) extends WebSocketPara//img: Int

  case class ReJoinGamePara(playerId: String, playerName: String, mode: Int = 0, img: Int = 0) extends WebSocketPara//img: Int

  case class CreateRoomPara(playerId: String, playerName: String, pwd: String = "", mode: Int = 0, img: Int = 0) extends WebSocketPara//img: Int

  case class WatchGamePara(roomId: String, playerId: String, accessCode: String) extends WebSocketPara

  case class WatchRecordPara(recordId: String, playerId: String, frame: String, accessCode: String) extends WebSocketPara

  case class joinRoomByIdPara(roomId: Int,playerId: String, playerName: String, pwd: String = "", mode: Int = 0, img: Int = 0) extends WebSocketPara
}
