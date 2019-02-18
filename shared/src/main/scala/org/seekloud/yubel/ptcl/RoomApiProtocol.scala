package org.seekloud.yubel.ptcl

import scala.collection.mutable

/**
  * User: Jason
  * Date: 2018/10/19
  * Time: 14:37
  */
object RoomApiProtocol {

  trait CommonRsp {
    val errCode: Int
    val msg: String
  }

  case class RecordReq(
                            recordId: Long
  )
  case class RecordListReq(
                            lastRecordId: Long,
                            count: Int
  )

  case class RecordByTimeReq(
                            startTime: Long,
                            endTime: Long,
                            lastRecordId: Long,
                            count: Int
  )

  case class RecordByPlayerReq(
                          playerId: String,
                          lastRecordId: Long,
                          count: Int
  )
  case class RecordListRsp(
                          data:List[recordInfo],
                          errCode: Int = 0,
                          msg: String = "ok"
  )

  case class recordInfo(
                          recordId: Long,
                          roomId: Long,
                          startTime: Long,
                          endTime: Long,
                          userCounts: Int,
                          userList: Seq[(String,String)]
  )

  case class RecordInfoReq(
                           recordId: Long,
                           playerId: String //正在观看玩家的ID
                           )

  case class RecordFrameInfo(
                            frame: Int,
                            frameNum: Int,
                            frameDuration: Int
                            )

  case class RecordPlayerList(
                               totalFrame: Int,
                               playerList: List[RecordPlayerInfo]
                             )

  case class ExistTime(
                           startFrame: Long,
                           endFrame: Long
                         )

  case class RecordPlayerInfo(
                             playerId: String,
                             nickname: String,
                             existTime: List[ExistTime]
                             )

  case class PlayerIdInfo(
                           playerId: String
                         )

  case class CreateRoomInfo(
                           mode: Int,
                           pwd: Option[String]
                         )

  case class PlayerInfo(
                         playerList: List[PlayerIdName]
                       )

  case class PlayerIdName(
                        playerId: String,
                        nickname: String
  )

  case class RoomIdInfo(
                         roomId: Int
                       )

  case class RoomIdReq(
                        roomId: Int
                      )
  case class PwdReq(
                        roomId: Int,
                        pwd: String
                      )
  case class AllRoomReq(
                        data: String
  )

  case class RoomIdRsp(
                        data: RoomIdInfo,
                        errCode: Int = 0,
                        msg: String = "ok"
                      )

  case class PlayerListRsp(
                            data: PlayerInfo,
                            errCode: Int = 0,
                            msg: String = "ok"
                          )

  case class RoomListRsp(
                          data: RoomListInfo,
                          errCode: Int = 0,
                          msg: String = "ok"
                        )

  case class RoomMapRsp(
                          data:RoomMapInfo,
                          errCode: Int = 0,
                          msg: String = "ok"
                        )

  case class RoomMapInfo(
                          roomMap: mutable.HashMap[Int, (Int, Option[String], mutable.HashSet[(String, String)])]
  )

  case class RoomListInfo(
                           roomList: List[Int]
                         )

  case class RoomListRsp4Client(
                          data: RoomListInfo4Client,
                          errCode: Int = 0,
                          msg: String = "ok"
                        ) extends CommonRsp

  case class C(
                errCode: Int = 0,
                msg: String = "ok"
              )


  case class RoomListInfo4Client(
                           roomList: List[String]
                         )

  case class RecordFrameRsp(
                        data: RecordFrameInfo,
                        errCode: Int = 0,
                        msg: String = "ok"
                      ) extends CommonRsp

  case class RecordPlayerInfoRsp(
                                data: RecordPlayerList,
                                errCode: Int = 0,
                                msg: String = "ok"
                                ) extends CommonRsp

  final case class ErrorRsp(
                             errCode: Int,
                             msg: String
                           ) extends CommonRsp

  final case class SuccessRsp(
                               errCode: Int = 0,
                               msg: String = "ok"
                             ) extends CommonRsp


}
