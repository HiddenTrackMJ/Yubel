package org.seekloud.yubel.utils

import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType

object WarningDialog {
  def initWarningDialog(context:String) = {
    val alert = new Alert(AlertType.WARNING)
    alert.setTitle("警告")
    alert.setHeaderText("")
    alert.setContentText(context)
    alert.showAndWait()
  }

//  def main(args: Array[String]): Unit = {
//    initWarningDialog("just test")
//  }
}
