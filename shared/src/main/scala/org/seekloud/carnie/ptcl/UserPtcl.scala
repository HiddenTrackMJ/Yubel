package org.seekloud.carnie.ptcl

/**
  * User: Jason
  * Date: 2019/2/17
  * Time: 15:52
  */
object UserPtcl {
  trait CommonRsp {
    val errCode: Int
    val msg: String
  }

  final case class ErrorRsp(
                             errCode: Int,
                             msg: String
                           ) extends CommonRsp

  final case class SuccessRsp(
                               errCode: Int = 0,
                               msg: String = "ok"
                             ) extends CommonRsp

  case class IdRsp(
                    id: Long,
                    errCode: Int = 0,
                    msg: String = "ok"
                  ) extends CommonRsp


  case class LoginReq(
                       name: String,
                       passWord: String
                     )

  case class AddUserReq(
                         userName:String,
                         securePwd:String
                       )



}
