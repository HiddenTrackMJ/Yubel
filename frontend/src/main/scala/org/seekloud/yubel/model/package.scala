package org.seekloud.yubel

/**
  * Created by haoshuhan on 2018/10/23.
  */
package object model {

  case class ReplayInfo(recordId: String, playerId: String, frame: String, accessCode:String)
}
