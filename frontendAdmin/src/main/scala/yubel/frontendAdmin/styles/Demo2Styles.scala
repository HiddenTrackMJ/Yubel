package org.seekloud.yubel.frontendAdmin.styles

/**
  * User: Jason
  * Date: 2018/12/19
  * Time: 9:55
  */
import scala.language.postfixOps
import scalacss.DevDefaults._



object Demo2Styles extends StyleSheet.Inline{

  import dsl._

  val menuHorizontalLink = style(
    cursor.pointer,
    display.inlineBlock,
    textDecorationLine.none,
    paddingLeft(15 px),
    paddingRight(15 px),
    paddingTop(5 px),
    paddingBottom(5 px),
    &.hover(
      backgroundColor(c"#CDCD00")
    )
  )

  val commonBtn = style(
    color(white),
    width(60 px),
    height(25 px),
    backgroundColor(blue),
    borderRadius(25 px),
    padding(5 px),
    marginLeft(20 px),
    border.none
  )

  val fixedBoxAdmin=style(
    width(100.%%),
    height(100.%%),
    position.fixed,
    backgroundColor(rgba(0,0,0,0.3)),
    top(0 px),
    zIndex(1),
  )
  val fixedNewAdmin = style(
    width(500 px),
    height(550 px),
    margin(60 px, auto, 0 px, auto),
    borderRadius(8 px),
    fontSize(18 px),
    backgroundColor(white),
    zIndex(2),
    alignItems.center
  )

  val inputStyle = style(
    marginLeft(50 px),
    marginTop(10 px),
    padding(5 px),
    borderRadius(5 px)
  )

  val inputbox = style(
    borderRadius(5 px),
    backgroundColor(c"#0FF")
  )
  val btnStyle = style(
    marginTop(10 px),
    marginLeft(100 px)
  )

}
