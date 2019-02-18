package org.seekloud.utils.essf

import java.io.File

import org.seekloud.yubel.common.AppSettings
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.byteobject.ByteObject._
import org.seekloud.byteobject.MiddleBufferInJvm
import org.seekloud.essf.io.FrameOutputStream



/**
  * Created by haoshuhan on 2018/10/11.
  */
object RecordGame {
  val utf8 = "utf-8"

  def getRecorder(fileName:String, index:Int, gameInformation: GameInformation, initStateOpt:Option[Snapshot] = None): FrameOutputStream = {
    val middleBuffer = new MiddleBufferInJvm(10 * 4096)
    val name = "carnie"
    val version = "0.1"
    val dir = new File(AppSettings.gameDataDirectoryPath)
    if(!dir.exists()){
      dir.mkdir()
    }
    val file = AppSettings.gameDataDirectoryPath + fileName + s"_$index"
    val gameInformationBytes = gameInformation.fillMiddleBuffer(middleBuffer).result()
    val initStateBytes = initStateOpt.map{
      case t: Snapshot =>
        t.fillMiddleBuffer(middleBuffer).result()
    }.getOrElse(Array[Byte]())
    val recorder = new FrameOutputStream(file)
    recorder.init(name,version,gameInformationBytes,initStateBytes)
    recorder
  }

}
