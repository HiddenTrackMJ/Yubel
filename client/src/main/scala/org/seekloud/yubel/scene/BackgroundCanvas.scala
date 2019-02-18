package org.seekloud.yubel.scene

import org.seekloud.yubel.paperClient.BorderSize
import javafx.scene.canvas.Canvas
import javafx.scene.paint.Color

/**
  * Created by dry on 2018/10/29.
  **/
class BackgroundCanvas(canvas: Canvas) {

  private val ctx = canvas.getGraphicsContext2D
  private val canvasUnit = 20


//  def drawCache(offx: Float, offy: Float): Unit = { //离屏缓存的更新--缓存边界
//    ctx.clearRect(0,0,canvas.getWidth,canvas.getHeight)
//    ctx.setFill(Color.rgb(105,105,105))
//
//    //画边界
//    ctx.fillRect(offx, offy, canvasUnit * BorderSize.w, canvasUnit)
//    ctx.fillRect(offx, offy, canvasUnit, canvasUnit * BorderSize.h)
//    ctx.fillRect(offx, BorderSize.h * canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
//    ctx.fillRect(BorderSize.w * canvasUnit, offy, canvasUnit, canvasUnit * (BorderSize.h + 1))
//  }

  def resetScreen(viewWidth:Int, viewHeight:Int) = {
    canvas.setWidth(viewWidth)
    canvas.setHeight(viewHeight)
  }

  def drawCache(): Unit = { //离屏缓存的更新--缓存边界
    ctx.setFill(Color.rgb(105,105,105))

    //画边界
    ctx.fillRect(0, 0, canvasUnit * BorderSize.w, canvasUnit)
    ctx.fillRect(0, 0, canvasUnit, canvasUnit * BorderSize.h)
    ctx.fillRect(0, BorderSize.h * canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
    ctx.fillRect(BorderSize.w * canvasUnit, 0, canvasUnit, canvasUnit * (BorderSize.h + 1))
  }
  def drawCache1(): Unit = { //离屏缓存的更新--缓存边界
    ctx.clearRect(0,0,canvas.getWidth,canvas.getHeight)
    ctx.setFill(Color.rgb(105,105,105))

    //画边界
    ctx.fillRect(0, 0,canvasUnit * BorderSize.w, canvasUnit)
    ctx.fillRect(0, 0, canvasUnit, canvasUnit * BorderSize.h)
    ctx.fillRect(0, BorderSize.h * canvasUnit, canvasUnit * (BorderSize.w + 1), canvasUnit)
    ctx.fillRect(BorderSize.w * canvasUnit, 0, canvasUnit, canvasUnit * (BorderSize.h + 1))
  }

}
