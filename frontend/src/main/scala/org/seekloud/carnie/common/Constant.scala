package org.seekloud.carnie.common

import org.scalajs.dom.ext.KeyCode

/**
  * Created by dry on 2018/9/3.
  **/
object Constant {

  val watchKeys = Set(
    KeyCode.Space,
    KeyCode.Left,
    KeyCode.Up,
    KeyCode.Right,
    KeyCode.Down,
    KeyCode.Q,
    KeyCode.W,
    KeyCode.E,
    KeyCode.R
  )

  object ColorsSetting {
    val backgroundColor = "#F5F5F5"
    val fontColor2 = "#000000"
    val gameNameColor = "#5BC48C"
    val fontColor = "#E0EEFD"
    val defaultColor = "#000080"
    val borderColor = "#696969"
    val mapColor = "#C0C0C0"
//    val gradeColor = "#3358FF"
    val redColor = "#FF0000"
    val greenColor = "#00FF00"
    val yellowColor = "#FFFF00"
    val darkYellowColor = "#EBEB68"
    val backgroundColor2 = "#333333"
    val fontColor3 = "#A9A9A9"
  }

  def shortMax(a: Short, b: Short): Short ={
    if(a > b) a else b
  }



}
