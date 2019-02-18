package org.seekloud.yubel.frontendAdmin.util

/**
  * Created by dry on 2018/10/12.
  **/

import scala.xml.Elem
import scala.language.implicitConversions

trait Component {

  def render: Elem

}

object Component {
  implicit def component2Element(comp: Component): Elem = comp.render
}
