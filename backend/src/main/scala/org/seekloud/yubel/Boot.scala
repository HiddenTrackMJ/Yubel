package org.seekloud.yubel

import akka.actor.{ActorSystem, Scheduler}
import akka.actor.typed.ActorRef
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.util.Timeout
import org.seekloud.yubel.core.RoomManager
import org.seekloud.yubel.http.HttpService
import akka.actor.typed.scaladsl.adapter._

import scala.language.postfixOps
import org.seekloud.yubel.core._
import akka.actor.typed.scaladsl.adapter._
import akka.dispatch.MessageDispatcher


/**
  * User: Taoz
  * Date: 8/26/2016
  * Time: 10:25 PM
  */
object Boot extends HttpService {

  import concurrent.duration._
  import org.seekloud.yubel.common.AppSettings._


  override implicit val system: ActorSystem = ActorSystem("yubel", config)
  // the executor should not be the default dispatcher.
  override implicit val executor: MessageDispatcher = system.dispatchers.lookup("akka.actor.my-blocking-dispatcher")

  override implicit val materializer : ActorMaterializer = ActorMaterializer()

  implicit val scheduler : Scheduler= system.scheduler

  implicit val timeout: Timeout = Timeout(20 seconds) // for actor asks

  val log: LoggingAdapter = Logging(system, getClass)

  override val roomManager: ActorRef[RoomManager.Command] =system.spawn(RoomManager.create(),"roomManager")

  def main(args: Array[String]) {
    log.info("Starting.")
    Http().bindAndHandle(routes, httpInterface, httpPort)
    log.info(s"Listen to the $httpInterface:$httpPort")
    log.info("Done.")
  }


}
