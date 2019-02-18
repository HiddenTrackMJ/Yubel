package org.seekloud.yubel.common

import com.typesafe.config.ConfigFactory

/**
  * Created by haoshuhan on 2018/12/12.
  */
object BotAppSetting {
  val botConfig = ConfigFactory.parseResources("bot.conf").withFallback(ConfigFactory.load())

  val appConfig = botConfig.getConfig("app")

  val userConfig = appConfig.getConfig("user")
  val email = userConfig.getString("email")
  val psw = userConfig.getString("psw")

  val render = appConfig.getBoolean("render")
  val isViewObservation = appConfig.getBoolean("isViewObservation")
  val isGray = appConfig.getBoolean("isGray")

  val apiToken = appConfig.getString("apiToken")

  val botInfo = appConfig.getConfig("botInfo")
  val botId = botInfo.getString("botId")
  val botKey = botInfo.getString("botKey")


}
