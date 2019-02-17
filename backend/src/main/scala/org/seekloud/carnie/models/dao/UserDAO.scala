package org.seekloud.carnie.models.dao

import org.seekloud.utils.DBUtil.db
import slick.jdbc.PostgresProfile.api._
import org.seekloud.carnie.models.SlickTables._

import scala.concurrent.Future
/**
  * User: Jason
  * Date: 2019/2/17
  * Time: 14:59
  */
object UserDAO {

  def getAllUser = {
    db.run{
      tUsers.result
    }
  }

  def isExist(name: String):Future[Boolean] = {
    db.run{
      tUsers.filter(_.username === name).exists.result
    }
  }
  def addUser(name:String, pwd:String, createTime:Long, state:Int):Future[Int] =
    db.run{
      tUsers += rUsers(-1l, name, pwd, createTime, state)
    }

  def deleteUser(name:String):Future[Int] =
    db.run{
      tUsers.filter(_.username === name).delete
    }

  def getIdPwdState(name: String): Future[Option[(Long,String,Int)]] ={
    db.run{
      tUsers.filter(_.username === name).map(i => (i.id,i.securePwd,i.state)).result.headOption
    }
  }

  def updateUser(name: String, pwd: String, state: Int): Future[Int] = {
    db.run{
      tUsers.filter(_.username === name).map(i => (i.username,i.securePwd,i.state)).update(name,pwd,state)
    }
  }


  def main(args: Array[String]): Unit = {
//    addUser(1,"test","123456",6514564,1)
    val a = getIdPwdState("test")
//    modifyState("test", 1)
    Thread.sleep(5000)
    println(a)
  }

}
