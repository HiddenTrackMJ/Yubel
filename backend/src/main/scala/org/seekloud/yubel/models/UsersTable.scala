package org.seekloud.yubel.models

import org.seekloud.utils.EhCacheApi

import scala.concurrent.Future
import org.seekloud.yubel.Boot.executor

/**
  * User: Jason
  * Date: 2019/2/22
  * Time: 11:46
  */
case class Users(id: Long, username: String, securePwd: String, createTime: Long, state: Int)

trait UsersTable {
  import org.seekloud.utils.DBUtil.driver.api._

  class UsersTable(tag: Tag) extends Table[Users](tag, "Users") {
    val id: Rep[Long] = column[Long]("id", O.AutoInc, O.PrimaryKey)
    val username: Rep[String] = column[String]("username", O.Length(255,varying=true))
    val securePwd: Rep[String] = column[String]("secure_pwd", O.Length(255,varying=true))
    val createTime: Rep[Long] = column[Long]("create_time")
    val state: Rep[Int] = column[Int]("state", O.Default(0))


    def * = (id, username, securePwd, createTime, state) <> (Users.tupled, Users.unapply)
  }

  protected val usersTableQuery = TableQuery[UsersTable]

}

object UsersRepo extends UsersTable {

  import org.seekloud.utils.DBUtil.driver.api._
  import org.seekloud.utils.DBUtil.db
//  private val usersCache = EhCacheApi.createCache[List[Users]]("usersCache", 1200, 1200)

  def create(): Future[Unit] = {
    db.run(usersTableQuery.schema.create)
  }


//  def getAllUsers: Future[List[Users]] = {
//    usersCache.apply(s"usersInfo", () =>
//      db.run (usersTableQuery.to[List].result)
//    )
//  }
//
  def updateUsers(userInfo: Users): Future[Int] = {
//    usersCache.remove("usersInfo")
    db.run(usersTableQuery.insertOrUpdate(userInfo))
  }
//
//  def deleteUser(name: String): Future[Int] = {
//    usersCache.remove("usersInfo")
//    db.run(usersTableQuery.filter(i => i.username === name).delete)
//  }
//
//  def getIdPwdState(name: String): Future[Option[(Long,String,Int)]] = {
//    db.run (usersTableQuery.filter(i => i.username === name).map(i => (i.id,i.securePwd,i.state)).result.headOption)
//  }

  def getAllUser = {
    db.run{
      usersTableQuery.result
    }
  }

  def isExist(name: String):Future[Boolean] = {
    db.run{
      usersTableQuery.filter(_.username === name).exists.result
    }
  }
  def addUser(name:String, pwd:String, createTime:Long, state:Int):Future[Int] =
    db.run{
      usersTableQuery += Users(-1l, name, pwd, createTime, state)
    }

  def deleteUser(name:String):Future[Int] =
    db.run{
      usersTableQuery.filter(_.username === name).delete
    }

  def getIdPwdState(name: String): Future[Option[(Long,String,Int)]] ={
    db.run{
      usersTableQuery.filter(_.username === name).map(i => (i.id,i.securePwd,i.state)).result.headOption
    }
  }

  def updateUser(name: String, pwd: String, state: Int): Future[Int] = {
    db.run{
      usersTableQuery.filter(_.username === name).map(i => (i.username,i.securePwd,i.state)).update(name,pwd,state)
    }
  }

  def main(args: Array[String]): Unit = {
//    updateUsers(Users(-1,"test","123456",6514564,1))
//    addUser("test","123456",6514564,1)
    val a = getAllUser
//    //    modifyState("test", 1)
    Thread.sleep(5000)
    println(a)
  }
}
