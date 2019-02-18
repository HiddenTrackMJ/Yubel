package org.seekloud.yubel.core

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, StashBuffer, TimerScheduler}
import org.seekloud.yubel.common.AppSettings
import org.seekloud.yubel.core.RoomActor.{UserActionOnServer, dispatch}
import org.seekloud.yubel.paperClient.Protocol.{Key, PressSpace}
import org.seekloud.yubel.paperClient.{Body,Border, Field, GridOnServer, Point, Protocol}
import org.slf4j.LoggerFactory

import concurrent.duration._
import scala.util.Random

/**
  * Created by dry on 2018/12/17.
  **/
object BotActor {
  //简单的bot：在某个房间生成后生成陪玩bot
  private val log = LoggerFactory.getLogger(this.getClass)

  sealed trait Command

  case class InitInfo(botName: String, mode: Int, grid: GridOnServer, roomActor: ActorRef[RoomActor.Command]) extends Command

  case class MakeAction(a: Int, state: Int) extends Command

  case class MakeMiniAction(point: Point, state: Int, isRefrain: Boolean = false) extends Command

  case object KillBot extends Command

  case object BotDead extends Command

  case object Space extends Command

  case object BackToGame extends Command

  private final case object MakeActionKey

  private final case object MakeMiniActionKey

  private final case object SpaceKey

//  private var action = 0


  def create(botId: String): Behavior[Command] = {
    implicit val stashBuffer: StashBuffer[Command] = StashBuffer[Command](Int.MaxValue)
    Behaviors.withTimers[Command] { implicit timer =>

      Behaviors.receive[Command] { (ctx, msg) =>
        msg match {
          case InitInfo(botName, mode, grid, roomActor) =>
            val frameRate = mode match {
              case 2 => Protocol.frameRate2
              case _ => Protocol.frameRate1
            }
            roomActor ! RoomActor.JoinRoom4Bot(botId, botName, ctx.self, new Random().nextInt(6))
            val randomTime = 1 + scala.util.Random.nextInt(20)
            timer.startSingleTimer(MakeActionKey, MakeAction(0, 0), randomTime * frameRate.millis)
            timer.startSingleTimer(MakeMiniActionKey, MakeMiniAction(Point(0,0),1),  (randomTime + 1) * frameRate.millis)
            gaming(botId, grid, roomActor, frameRate)

          case unknownMsg@_ =>
            log.warn(s"${ctx.self.path} unknown msg: $unknownMsg")
            stashBuffer.stash(unknownMsg)
            Behaviors.unhandled
        }
      }
    }
  }

  def gaming(botId: String, grid: GridOnServer, roomActor: ActorRef[RoomActor.Command], frameRate: Int, stateForA: Int = 0, stateForMA: Int = 0)
            (implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case MakeAction(a, state) =>
          val rTime = 1 + scala.util.Random.nextInt(15)
          if (state == stateForMA - 1) {
            val nextState = state + 1
            timer.startSingleTimer(MakeActionKey, MakeAction(rTime, nextState), (rTime + Random.nextInt(a+1)) * frameRate.millis)
          }
          else {
            timer.startSingleTimer(MakeActionKey, MakeAction(rTime, state), (rTime + Random.nextInt(a+1)) * frameRate.millis)
            var actionCode = (stateForA % 4 + 37).toByte
            if(grid.snakes.exists(_._1 == botId)) {
              //            val header = grid.snakes.find(_._1 == botId).get._2.header
              val direction = grid.snakes.find(_._1 == botId).get._2.direction
              actionCode = pointsToAction(directionToRight(direction))
            }
            if(grid.snakes.exists(_._1 == botId)){
              val header = grid.snakes.find(_._1 == botId).get._2.header
              val direction = grid.snakes.find(_._1 == botId).get._2.direction
              val rightDirection = directionToRight(direction)
              //            log.info(s"=====bot direction:$direction")
              val newHeader = List(3,2,1).map(header + rightDirection * _)
              actionCode = pointsToAction(rightDirection)
              var flag = true
              newHeader.foreach{ h =>
                grid.grid.get(h) match {
                  case Some(Body(bid, _)) if flag =>
                    actionCode = pointsToAction(direction)
                    flag = false
                  case Some(Border) if flag =>
                    actionCode = pointsToAction(direction)
                    flag = false
                  case _  =>
                }
              }
              val newHeaderStraight = List(4,3,2,1).map(header + direction * _)
              flag = true
              newHeaderStraight.foreach{ h =>
                grid.grid.get(h) match {
                  case Some(Field(id)) if flag && id == botId =>
                    actionCode = pointsToAction(direction)
                    flag = false
                  case _  =>
                }
              }
            }
            if (stateForA == 0) actionCode = (Random.nextInt(4) + 37).toByte
            roomActor ! UserActionOnServer(botId, Key(actionCode, grid.frameCount, -1))
          }

//          actionNum match {
//            case 0 => gaming(botId, grid, roomActor, frameRate, actionNum + 1)
//            case _ => gaming(botId, grid, roomActor, frameRate, actionNum + 1)
//          }
          gaming(botId, grid, roomActor, frameRate, stateForA + 1, stateForMA)

        case MakeMiniAction(a@Point(x,y), state, isRefrain) =>
          var actionCode: Byte = 32
          var stateForMini = stateForMA
          var isRe = isRefrain
          var currentDirection = a
          if (state == stateForA - 1) {
            val nextState = state + 1
            timer.startSingleTimer(MakeMiniActionKey, MakeMiniAction(a + actionToPoints(actionCode),nextState,isRe),  frameRate.millis)
          }
          else {
            stateForMini = stateForMA + 1
            if(grid.snakes.exists(_._1 == botId)){
              val header = grid.snakes.find(_._1 == botId).get._2.header
              val direction = grid.snakes.find(_._1 == botId).get._2.direction
              currentDirection = direction
              val rightDirection = directionToRight(direction)
              val leftDirection = actionToPoints(pointsToAvoid(direction))
              //            log.info(s"=====bot direction:$direction")
              val newHeader = List(3,2,1).map(header + direction * _)
              var flag = true
              newHeader.foreach{ h =>
                grid.grid.get(h) match {
                  case Some(Field(fid)) if fid == botId && h == header + direction =>
                    isRe = false
                  case Some(Border) if flag=>
                    actionCode = pointsToAction(rightDirection)
                    flag = false

                  case Some(Field(fid)) if fid == botId && flag =>
                    actionCode = pointsToAction(direction)
                    flag = false

//                  case Some(Body(bid, _)) if bid != botId && flag && bid.take(3) == "bot" =>
//                    actionCode = pointsToAction(rightDirection)
//                    flag = false

                  case _  if flag => actionCode = pointsToAction(direction)

                  case _ =>
                }
              }
              val isInField = grid.grid.get(header - direction) match {
                case Some(Field(id)) if id == botId => true
                case _ => false
              }
              if (!isInField) {
                var flag = true
//                if (!isRefrain){
                  isRe = true
                  val newHeaderRight = (1 to 4).toList.reverse.map(header + rightDirection * _)
                  newHeaderRight.foreach{ h =>
                    grid.grid.get(h) match {
                      case Some(Field(fid)) if fid == botId && flag =>
                        actionCode = pointsToAction(rightDirection)
                        flag = false
                      case Some(Body(bid, _)) if bid == botId  =>
                        actionCode = pointsToAction(direction)
                      case Some(Body(bid, _)) if bid != botId && bid.take(3) == "bot" =>
                        actionCode = pointsToAction(direction)
                      case _ =>
                    }
                  }
                  val newHeaderLeft = (1 to 10).toList.reverse.map(header + actionToPoints(pointsToAvoid(direction)) * _)
                  flag = true
                  newHeaderLeft.foreach{ h =>
                    grid.grid.get(h) match {
                      case Some(Field(fid)) if fid == botId && flag =>
                        actionCode = pointsToAvoid(direction)
                        flag = false
                      case Some(Body(bid, _)) if bid == botId  =>
                        actionCode = pointsToAction(direction)
                      case Some(Body(bid, _)) if bid != botId && bid.take(3) == "bot" =>
                        actionCode = pointsToAction(direction)
                      case _ =>
                    }
                  }
//                }
                flag = true
                newHeader.foreach { h =>
                  grid.grid.get(h) match {
                    case Some(Body(bid, _)) if bid == botId && flag =>
                      actionCode = pointsToAvoid(direction)
                      flag = false
//                    case Some(Body(bid, _)) if bid != botId && flag && bid.take(3) == "bot" =>
//                      actionCode = pointsToAction(rightDirection)
//                      flag = false
                    case _ =>
                  }
                }
              }

            }
            timer.startSingleTimer(MakeMiniActionKey, MakeMiniAction(a + actionToPoints(actionCode),state,isRe),  frameRate.millis)
            if(actionCode != pointsToAction(currentDirection))
            roomActor ! UserActionOnServer(botId, Key(actionCode, grid.frameCount, -1))
          }

          gaming(botId, grid, roomActor, frameRate, stateForA, stateForMini)

        case BotDead =>
//          log.info(s"bot dead:$botId")
          timer.startSingleTimer(SpaceKey, Space, (2 + scala.util.Random.nextInt(8)) * frameRate.millis)
          timer.cancel(MakeActionKey)
          timer.cancel(MakeMiniActionKey)
          dead(botId, grid, roomActor, frameRate)

        case KillBot =>
          log.debug(s"botActor:$botId go to die...")
          Behaviors.stopped

        case unknownMsg@_ =>
          log.warn(s"${ctx.self.path} unknown msg: $unknownMsg,be 111")
          stashBuffer.stash(unknownMsg)
          Behaviors.unhandled
      }
    }
  }

  def dead(botId: String, grid: GridOnServer, roomActor: ActorRef[RoomActor.Command], frameRate: Int)
          (implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case Space =>
//          log.info(s"recv Space: botId:$botId")
//          val actionCode = 32
          roomActor ! UserActionOnServer(botId, PressSpace)
          Behaviors.same

        case BackToGame =>
//          log.info(s"back to game: botId:$botId")
          val randomTime = 2 + scala.util.Random.nextInt(20)
          timer.startSingleTimer(MakeActionKey, MakeAction(0, 0), randomTime * frameRate.millis)
          timer.startSingleTimer(MakeMiniActionKey, MakeMiniAction(Point(0,0),1),  (randomTime + 1) * frameRate.millis)
          gaming(botId, grid, roomActor, frameRate)

        case KillBot =>
//          log.debug(s"botActor:$botId go to die...")
          Behaviors.stopped

        case unknownMsg@_ =>
          log.warn(s"${ctx.self.path} unknown msg: $unknownMsg,be 222")
          stashBuffer.stash(unknownMsg)
          Behaviors.unhandled
      }
    }
  }

  def directionToLeft(p: Point): Point ={
    Point(0,0) - directionToRight(p)
  }

  def directionToRight(p: Point): Point ={
    p match {
      case Point(1,0) => Point(0,1)
      case Point(-1,0) => Point(0,-1)
      case Point(0,1) => Point(-1,0)
      case Point(0,-1) => Point(1,0)
      case _ => Point(0,0)
    }
  }

  def pointsToAction(p: Point):Byte  ={
    p match {
      case Point(1,0) => 39
      case Point(-1,0) => 37
      case Point(0,1) => 40
      case Point(0,-1) => 38
      case _ => 32
    }
  }

  def actionToPoints(a: Int):Point  ={
    a match {
      case 39 => Point(1,0)
      case 37 => Point(-1,0)
      case 40 => Point(0,1)
      case 38 => Point(0,-1)
      case _ => Point(0,0)
    }
  }

  def pointsToAvoid(p: Point):Byte  ={
    val a = List(38,40)
    val b = List(37,39)
    val r = Random.nextInt(2)
    p match {
      case Point(1,0) => 38
      case Point(-1,0) => 40
      case Point(0,1) => 39
      case Point(0,-1) => 37
      case _ => 32
    }
  }
}
