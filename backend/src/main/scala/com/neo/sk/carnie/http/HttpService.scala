package com.neo.sk.carnie.http

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{Materializer, OverflowStrategy}
import akka.util.Timeout
import scala.concurrent.ExecutionContextExecutor

/**
  * User: Taoz
  * Date: 8/26/2016
  * Time: 10:27 PM
  */
trait HttpService extends PlayerService
  with ResourceService
  with EsheepService
  with RoomApiService
  with adminService{


  implicit val system: ActorSystem

  implicit val executor: ExecutionContextExecutor

  implicit val materializer: Materializer

  implicit val timeout: Timeout


  val routes =
    pathPrefix("carnie") {
      netSnakeRoute ~
      resourceRoutes ~
      esheepRoute ~
      roomApiRoutes ~
      adminRoutes ~
      getFromResource("html/netSnake.html")
    }



  def tmp = {
    val out = Source.empty
    val in = Sink.ignore
    Flow.fromSinkAndSource(in, out)
  }


  def tmp2 = {

    val sink = Sink.ignore
    def chatFlow(sender: String): Flow[String, String, Any] = {
      val in =
        Flow[String]
          .to(sink)

      // The counter-part which is a source that will create a target ActorRef per
      // materialization where the chatActor will send its messages to.
      // This source will only buffer one element and will fail if the client doesn't read
      // messages fast enough.
      val chatActor: ActorRef = null
      val out =
        Source.actorRef[String](1, OverflowStrategy.fail)
          .mapMaterializedValue(actor => chatActor ! "NewParticipant(sender, _)")

      Flow.fromSinkAndSource(in, out)
    }
  }


}
