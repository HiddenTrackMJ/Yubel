package org.seekloud.utils

import slick.jdbc.PostgresProfile.api._
import org.slf4j.LoggerFactory
import com.zaxxer.hikari.HikariDataSource
import slick.jdbc.H2Profile
/**
  * User: Jason
  * Date: 2018/10/22
  * Time: 14:50
  */





object DBUtil {
  val log = LoggerFactory.getLogger(this.getClass)
  private val dataSource = createDataSource()

  import org.seekloud.yubel.common.AppSettings._

  private def createDataSource() = {

    val dataSource = new org.h2.jdbcx.JdbcDataSource

    //val dataSource = new MysqlDataSource()

    log.info(s"connect to db: $slickUrl")
    dataSource.setUrl(slickUrl)
    dataSource.setUser(slickUser)
    dataSource.setPassword(slickPassword)

    val hikariDS = new HikariDataSource()
    hikariDS.setDataSource(dataSource)
    hikariDS.setMaximumPoolSize(slickMaximumPoolSize)
    hikariDS.setConnectionTimeout(slickConnectTimeout)
    hikariDS.setIdleTimeout(slickIdleTimeout)
    hikariDS.setMaxLifetime(slickMaxLifetime)
    hikariDS.setAutoCommit(true)
    hikariDS
  }

  val driver = H2Profile

  import driver.api.Database

  val db: Database = Database.forDataSource(dataSource, Some(slickMaximumPoolSize))




}