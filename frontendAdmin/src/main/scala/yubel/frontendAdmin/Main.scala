package org.seekloud.yubel.frontendAdmin

import mhtml.{Cancelable, Rx, mount}
import org.scalajs.dom
import scala.xml.Elem
import org.seekloud.yubel.frontendAdmin.util.PageSwitcher
import org.seekloud.yubel.frontendAdmin.pages._
import org.seekloud.yubel.frontendAdmin.components.Header
import org.seekloud.yubel.frontendAdmin.styles._

/**
  * User: Jason
  * Date: 2018/12/17
  * Time: 17:27
  */
object Main {

  import scalacss.DevDefaults._
  styles.Demo2Styles.addToDocument()

  def main(args: Array[String]): Unit ={
    MainEnter.show()
  }

}

object MainEnter extends PageSwitcher {

  val currentPage: Rx[Elem] = currentHashVar.map {
    case Nil => LoginPage.render
    case "LoginPage" :: Nil => LoginPage.render
    case "View"  :: Nil => ViewPage.render
    case "CurrentDataPage"  :: Nil => CurrentDataPage.render
    case "UserInfoPage" :: Nil => UserInfoPage.render
    case _ => <div>Error Page</div>
  }

  val header: Rx[Elem] = currentHashVar.map {
    //    case Nil => Header.render
    case "View" :: Nil => Header.render
    case "CurrentDataPage" :: Nil => Header.render
    case "UserInfoPage" :: Nil => Header.render
    case _ => <div></div>
  }

  def show(): Cancelable = {
    val page =
      <div>
        {header}{currentPage}
      </div>
    switchPageByHash()
    mount(dom.document.body, page)
  }

}