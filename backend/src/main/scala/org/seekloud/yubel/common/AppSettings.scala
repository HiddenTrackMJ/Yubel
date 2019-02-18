package org.seekloud.yubel.common

import java.util.concurrent.TimeUnit

import org.seekloud.utils.SessionSupport.SessionConfig
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory

/**
  * User: Taoz
  * Date: 9/4/2015
  * Time: 4:29 PM
  */
object AppSettings {

  private implicit class RichConfig(config: Config) {
    val noneValue = "none"

    def getOptionalString(path: String): Option[String] =
      if (config.getAnyRef(path) == noneValue) None
      else Some(config.getString(path))

    def getOptionalLong(path: String): Option[Long] =
      if (config.getAnyRef(path) == noneValue) None
      else Some(config.getLong(path))

    def getOptionalDurationSeconds(path: String): Option[Long] =
      if (config.getAnyRef(path) == noneValue) None
      else Some(config.getDuration(path, TimeUnit.SECONDS))
  }


  val log = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.parseResources("product.conf").withFallback(ConfigFactory.load())

  val appConfig = config.getConfig("app")
  val limitNum = appConfig.getInt("limitNum")
  val upperLimit = appConfig.getString("upperLimit")
  val lowerLimit = appConfig.getString("lowerLimit")
  val decreaseRate = appConfig.getDouble("decreaseRate")
  val gameDataDirectoryPath = appConfig.getString("gameDataDirectoryPath")
  val minPlayerNum = appConfig.getInt("minPlayerNum")

  val httpInterface = appConfig.getString("http.interface")
  val httpPort = appConfig.getInt("http.port")
  val httpUrl = appConfig.getString("http.url")

  val botMap = {
    import collection.JavaConverters._
    val botIdList = appConfig.getStringList("bot.botId").asScala
    val botNames = appConfig.getStringList("bot.botName").asScala
    require(botIdList.length == botNames.length, "botIdList.length and botNames.length not equel.")
    botIdList.zip(botNames).toMap
  }
  val adminId = appConfig.getString("admin.Id")
  val adminPassWord = appConfig.getString("admin.passWord")



  val appSecureMap = {
    import collection.JavaConverters._
    val appIds = appConfig.getStringList("client.appIds").asScala
    val secureKeys = appConfig.getStringList("client.secureKeys").asScala
    require(appIds.length == secureKeys.length, "appIdList.length and secureKeys.length not equel.")
    appIds.zip(secureKeys).toMap
  }

  val esheepConfig = config.getConfig("dependence.esheep")
  val esheepProtocol = esheepConfig.getString("protocol")
  val esheepDomain = esheepConfig.getString("domain")
  val esheepUrl = esheepConfig.getString("url")
  val esheepGameId = esheepConfig.getLong("gameId")
  val esheepGsKey = esheepConfig.getString("gsKey")

  val slickConfig = config.getConfig("slick.db")
  val slickUrl = slickConfig.getString("url")
  val slickUser = slickConfig.getString("user")
  val slickPassword = slickConfig.getString("password")
  val slickMaximumPoolSize = slickConfig.getInt("maximumPoolSize")
  val slickConnectTimeout = slickConfig.getInt("connectTimeout")
  val slickIdleTimeout = slickConfig.getInt("idleTimeout")
  val slickMaxLifetime = slickConfig.getInt("maxLifetime")

  val essfMapKeyName = "essfMap"

  val sessionConfig = {
    val sConf = config.getConfig("session")
    SessionConfig(
      cookieName = sConf.getString("cookie.name"),
      serverSecret = sConf.getString("serverSecret"),
      domain = sConf.getOptionalString("cookie.domain"),
      path = sConf.getOptionalString("cookie.path"),
      secure = sConf.getBoolean("cookie.secure"),
      httpOnly = sConf.getBoolean("cookie.httpOnly"),
      maxAge = sConf.getOptionalDurationSeconds("cookie.maxAge"),
      sessionEncryptData = sConf.getBoolean("encryptData")
    )
  }

  val projectVersion = appConfig.getString("projectVersion")


}
