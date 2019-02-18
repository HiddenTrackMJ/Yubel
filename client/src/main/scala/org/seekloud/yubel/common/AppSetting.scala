package org.seekloud.yubel.common

import java.io.File
import com.typesafe.config.ConfigFactory

/**
  * Created by dry on 2018/10/23.
  **/
object AppSetting {

  val config = ConfigFactory.parseResources("product.conf").withFallback(ConfigFactory.load())

  val esheepConfig = config.getConfig("dependence.esheep")
  val esheepProtocol = esheepConfig.getString("protocol")
  val esheepDomain = esheepConfig.getString("domain")
  val esheepGameId = esheepConfig.getLong("gameId")
  val esheepGsKey = esheepConfig.getString("gsKey")

  val file = new File("bot.conf")
  if (file.isFile && file.exists) {
    val botConfig = ConfigFactory.parseResources("bot.conf").withFallback(ConfigFactory.load())

    val appConfig = botConfig.getConfig("app")

    val userConfig = appConfig.getConfig("user")
    val email = userConfig.getString("email")
    val psw = userConfig.getString("psw")

    val render = appConfig.getBoolean("render")
  }

}
