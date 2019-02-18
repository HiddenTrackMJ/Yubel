package org.seekloud.yubel.models.dao

import org.seekloud.utils.DBUtil.db
import slick.jdbc.PostgresProfile.api._
import org.seekloud.yubel.models.SlickTables._
import org.seekloud.yubel.Boot.executor

object PlayerRecordDAO {

  def addPlayerRecord(playerId:String, nickName:String, killing:Int, killed:Int, score:Float, startTime:Long, endTime:Long) =
    db.run{
      tPlayerRecord += rPlayerRecord(-1l, playerId, nickName, killing, killed, score, startTime, endTime)
    }

  def getPlayerRecord()={
    db.run{
      tPlayerRecord.sortBy(_.startTime.desc).result
    }
  }
}
