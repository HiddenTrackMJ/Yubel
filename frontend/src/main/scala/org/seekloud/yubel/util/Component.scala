package org.seekloud.yubel.util

/**
  * Created by dry on 2018/10/12.
  **/

import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement

import scala.xml.Elem

/**
  * User: Taoz
  * Date: 3/29/2018
  * Time: 1:59 PM
  */
trait Component {

  def render: Elem

}

