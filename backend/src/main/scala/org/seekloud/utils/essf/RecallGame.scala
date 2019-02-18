package org.seekloud.utils.essf

import org.seekloud.yubel.paperClient.Protocol._
import io.netty.handler.ssl.ApplicationProtocolConfig.Protocol
import org.seekloud.essf.io.{FrameData, FrameInputStream}
import org.seekloud.byteobject.ByteObject._
import org.seekloud.byteobject.{MiddleBuffer, MiddleBufferInJvm}

import scala.collection.mutable
/**
  * Created by haoshuhan on 2018/10/12.
  */
object RecallGame {

  def initInput(targetFile: String): FrameInputStream = {
    new FrameInputStream(targetFile)
  }

  /**解码*/

  def metaDataDecode(a:Array[Byte])={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[GameInformation](buffer)
  }

  def initStateDecode(a:Array[Byte]) ={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[Snapshot](buffer)
  }

  def userMapDecode(a:Array[Byte])={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[EssfMapInfo](buffer)
  }

  /**用于后端先解码数据然后再进行编码传输*/
  def replayEventDecode(a:Array[Byte]): GameEvent={
    if (a.length > 0) {
      val buffer = new MiddleBufferInJvm(a)
      bytesDecode[List[GameEvent]](buffer) match {
        case Right(r) =>
          EventData(r)
        case Left(e) =>
          println(s"decode events error:")
          DecodeError()
      }
    }else{
//      println(s"events None")
      DecodeError()
    }
  }

  def replayStateDecode(a:Array[Byte]): GameEvent={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[Snapshot](buffer) match {
      case Right(r) =>
//        println(s"states decode success")
        r
      case Left(e) =>
        println(s"decode states error:")
        DecodeError()
    }
  }

//  def userMapEncode(u:mutable.HashMap[EssfMapKey,EssfMapJoinLeftInfo])(implicit middleBuffer: MiddleBufferInJvm)={
//    EssfMapInfo(u.toList).fillMiddleBuffer(middleBuffer).result()
//  }

}
