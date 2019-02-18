package org.seekloud.yubel.http

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.{StatusCode, Uri}
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{Materializer, OverflowStrategy}
import akka.util.Timeout

import scala.concurrent.ExecutionContextExecutor
import akka.http.scaladsl.model.headers.{CacheDirective, `Cache-Control`}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `public`}

import org.seekloud.yubel.common.AppSettings.httpUrl

/**
  * User: Taoz
  * Date: 8/26/2016
  * Time: 10:27 PM
  */
trait HttpService extends PlayerService
  with ResourceService
  with EsheepService
  with RoomApiService
  with AdminService
  with UserService{


  implicit val system: ActorSystem

  implicit val executor: ExecutionContextExecutor

  implicit val materializer: Materializer

  implicit val timeout: Timeout

  private val home = get {
    redirect(Uri(s"$httpUrl"),StatusCodes.SeeOther)
  }

  val routes = ignoreTrailingSlash{
    pathPrefix("yubel") {
      netSnakeRoute ~
        resourceRoutes ~
        esheepRoute ~
        roomApiRoutes ~
        adminRoutes ~
        userRoutes
    } ~ home
  }
}
