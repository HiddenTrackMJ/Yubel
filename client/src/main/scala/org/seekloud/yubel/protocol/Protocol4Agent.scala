package org.seekloud.yubel.protocol

object Protocol4Agent {

  case class UrlData(
                      wsUrl: String,
                      scanUrl: String
                    )

  case class LoginRsp(
                       data: UrlData,
                       errCode: Int,
                       msg: String
                     )

  case class GameServerInfo(
                             ip: String,
                             port: Int,
                             domain: String
                           )

  case class LinkGameAgentData(
                                accessCode: String,
                                gsPrimaryInfo: GameServerInfo
                              )

  case class LinkGameAgentRsp(
                               data: LinkGameAgentData,
                               errCode: Int,
                               msg: String
                             )

  case class LinkGameAgentReq(
                               gameId: Long,
                               playerId: String
                             )

  case class BotKey2TokenReq(
                         botId: String,
                         botKey: String
                         )

  case class BotTokenData(
                         botName: String,
                         token: String,
                         expireTime: Long
                         )

  case class BotKey2TokenRsp(
                            data: BotTokenData,
                            errCode: Int,
                            msg: String
                            )

  case class RefreshTokenReq(
                              playerId: String
                            )

  sealed trait WsData

  case class Ws4AgentRsp(
                          data: UserInfo,
                          errCode: Int,
                          msg: String
                        ) extends WsData

  case object HeartBeat extends WsData

  case class UserInfo(
                       userId: Long,
                       nickname: String,
                       token: String,
                       tokenExpireTime: Long
                     )

  final case class LoginByMailReq(
                             email: String,
                             password: String
                           )

  final case class ESheepUserInfoRsp(
                                      userName: String,
                                      userId: Long,
                                      headImg: String,
                                      token: String,
                                      gender: Int,
                                      errCode: Int = 0,
                                      msg: String = "ok"
                                    )

}
