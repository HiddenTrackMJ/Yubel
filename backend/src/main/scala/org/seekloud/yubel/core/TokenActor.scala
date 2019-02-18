package org.seekloud.yubel.core

import scala.concurrent.duration._
import org.seekloud.yubel.common.AppSettings
import org.seekloud.yubel.Boot.executor
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import org.slf4j.LoggerFactory
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import org.seekloud.utils.EsheepClient
import org.seekloud.yubel.common.AppSettings

/**
  * Lty 18/10/17
  */
object TokenActor {

  private val log = LoggerFactory.getLogger(this.getClass)

  sealed trait Command

  case class GetToken(times: Int = 0) extends Command

  case class TimeOut(msg: String) extends Command

  private final case object BehaviorChangeKey

  private final case object GetTokenKey

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  final case class AskForToken(reply: ActorRef[String]) extends Command

  final case class AccessToken(token: String, expiresAt: Long) {
    def isOutOfTime: Boolean = System.currentTimeMillis() > expiresAt
  }

  val behaviors: Behavior[Command] = init()

  def init(): Behavior[Command] = {
    Behaviors.setup[Command] { ctx =>
      implicit val stashBuffer: StashBuffer[Command] = StashBuffer[Command](Int.MaxValue)
      Behaviors.withTimers[Command] { implicit timer =>
        ctx.self ! GetToken()
        updateToken(Nil)
      }
    }
  }

  def idle(token: AccessToken)(implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case AskForToken(reply) =>
          if (token.isOutOfTime) {
//            log.debug("time to refresh token...")
            ctx.self ! GetToken()
            switchBehavior(ctx, "updateToken", updateToken(List(reply)))
          } else {
            reply ! token.token
            Behaviors.same
          }

        case unknownMsg@_ =>
//          log.warn(s"${ctx.self.path} unknown msg: $unknownMsg")
          stashBuffer.stash(unknownMsg)
          Behaviors.unhandled
      }
    }
  }

  def updateToken(tokenWaiter: List[ActorRef[String]])(implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case GetToken(times) =>
          if (times < 3) {
            EsheepClient.getTokenRequest(AppSettings.esheepGameId, AppSettings.esheepGsKey).map {
              case Right(rsp) =>
//                println(s"token is ${rsp.token}")
                tokenWaiter.foreach(r => r ! rsp.token)
                val expiresAt = System.currentTimeMillis() + 7200*1000 - 10*1000
                ctx.self ! SwitchBehavior("idle", idle(AccessToken(rsp.token, expiresAt)))

              case Left(e) =>
                timer.startSingleTimer(GetTokenKey, GetToken(times + 1), 5.seconds)
//                log.info(s"Some errors happened in getToken: $e")
            }
          } else {
//            log.warn("get token from esheep try over times...i try it again 5 minutes...")
            timer.startSingleTimer(GetTokenKey, GetToken(), 5.minutes)
          }
          Behaviors.same

        case AskForToken(reply) =>
          updateToken(reply :: tokenWaiter)

        case SwitchBehavior(name, behavior, durationOpt, timeOut) =>
          switchBehavior(ctx, name, behavior, durationOpt, timeOut)

        case unknownMsg@_ =>
          log.warn(s"${ctx.self.path} unknown msg: $unknownMsg")
          stashBuffer.stash(unknownMsg)
          Behaviors.unhandled
      }
    }
  }

  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer: TimerScheduler[Command]) = {
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey, timeOut, _))
    stashBuffer.unstashAll(ctx, behavior)
  }

}
