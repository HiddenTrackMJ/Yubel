package org.seekloud.yubel.paperClient

import org.seekloud.yubel.common.Constant.ColorsSetting
import org.seekloud.yubel.paperClient.Protocol.ReceivePingPacket
import org.scalajs.dom.CanvasRenderingContext2D

/**
  * Created by dry on 2018/9/3.
  **/
object PerformanceTool {
  //FPS
  private var lastRenderTime = System.currentTimeMillis()
  private var lastRenderTimes = 0
  private var renderTimes = 0
//  private var tempTime = System.currentTimeMillis()

  private def addFps() = {
    val time = System.currentTimeMillis()
    renderTimes += 1
//    println(s"addFps time:${time - tempTime}")
//    tempTime = time
    if (time - lastRenderTime > 1000) {
      lastRenderTime = time
      lastRenderTimes = renderTimes
      renderTimes = 0
    }
  }

  def renderFps(ctx: CanvasRenderingContext2D, leftBegin: Int, lineHeight: Int, startDrawTime: Long) = {
    addFps()
    addDrawTime(startDrawTime)
//    ctx.font = "14px Helvetica"
    ctx.textAlign = "start"
//    val fpsString = s"fps : $lastRenderTimes, ping: $latency"
//    ctx.fillText(fpsString, leftBegin, lineHeight)

    ctx.font = "15px Helvetica"
    ctx.fillStyle = ColorsSetting.fontColor2
    val fpsString = "fps : "
    val pingString = "ping: "
    val drawTimeString = "drawTimeAvg: "
    ctx.fillText(fpsString, leftBegin, lineHeight * 5)
    ctx.fillText(pingString, leftBegin + ctx.measureText(fpsString).width + 50, lineHeight * 5)//50
    ctx.fillText(drawTimeString, leftBegin, lineHeight * 6)
    ctx.strokeStyle = "black"
    ctx.strokeText(lastRenderTimes.toString, leftBegin + ctx.measureText(fpsString).width, lineHeight * 5)
    ctx.fillStyle = if (lastRenderTimes < 50) ColorsSetting.redColor else ColorsSetting.greenColor
    ctx.fillText(lastRenderTimes.toString, leftBegin + ctx.measureText(fpsString).width, lineHeight * 5)
//    ctx.strokeStyle = "black"
    ctx.strokeText(s"${latency}ms", leftBegin + ctx.measureText(fpsString).width + ctx.measureText(pingString).width + 60, lineHeight * 5)
    ctx.fillStyle = if (latency <= 100) ColorsSetting.greenColor else if (latency > 100 && latency <= 200) ColorsSetting.yellowColor else ColorsSetting.redColor
    ctx.fillText(s"${latency}ms", leftBegin + ctx.measureText(fpsString).width + ctx.measureText(pingString).width + 60, lineHeight * 5)
//    ctx.strokeStyle = "black"
    ctx.strokeText(s"${drawTimeAvg}ms".toString, leftBegin + ctx.measureText(drawTimeString).width, lineHeight * 6)
    ctx.fillStyle = if (drawTimeAvg > 10) ColorsSetting.redColor else ColorsSetting.greenColor
    ctx.fillText(s"${drawTimeAvg}ms".toString, leftBegin + ctx.measureText(drawTimeString).width, lineHeight * 6)
  }

  //drawTimeAverage
  private var drawTimeList: List[Long] = Nil
  private val drawTimes = 20
  private var drawTimeAvg: Long = 0l

  private def addDrawTime(startTime: Long): Unit = {
    val currentTime = System.currentTimeMillis()
    drawTimeList = (currentTime - startTime) :: drawTimeList
    if(drawTimeList.size >= drawTimes) {
      drawTimeAvg = drawTimeList.sum / drawTimeList.size
      drawTimeList = Nil
    }
  }

  //PING
  private var receiveNetworkLatencyList: List[Long] = Nil
  private val PingTimes = 4
  private var latency: Long = 0L

  def receivePingPackage(p: Long, currentTime: Long): Unit = {
//    val currentTime = System.currentTimeMillis()
    receiveNetworkLatencyList = (currentTime - p) :: receiveNetworkLatencyList
    if(currentTime - p < 0) {
      println(s"!!!!error:::::current time:$currentTime")
      println(s"p.createTime:$p")
    }

    if (receiveNetworkLatencyList.lengthCompare(PingTimes) >= 0) {
      latency = receiveNetworkLatencyList.sum / receiveNetworkLatencyList.size
      receiveNetworkLatencyList = Nil
    }
  }

}
