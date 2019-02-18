package org.seekloud.yubel.ptcl

object EsheepPtcl {

  case class PlayerMsg(
                      playerMsg: Map[String, String]
                      )

  case class GetBotReq4Client(
                               userId: Long,
                               lastId: Long,
                               count: Int
                             )

  case class BotListReq(
                       userId:Long,
                       lastId:Long
                       )

  case class BotInfo(
                      id: Long,
                      userId: String,
                      botName: String,
                      botDesc: String,
                      botKey: String,
                    )

  case class GetBotListRsp(
                            data: List[BotInfo],
                            errCode: Int = 0,
                            msg: String = "ok"
                          )
}
