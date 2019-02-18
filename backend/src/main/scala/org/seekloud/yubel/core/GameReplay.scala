package org.seekloud.yubel.core

import akka.actor.typed.{ActorRef, Behavior, PostStop}
import org.seekloud.yubel.paperClient.{Protocol, WsSourceProtocol}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import org.seekloud.yubel.common.AppSettings
import org.seekloud.yubel.models.dao.RecordDAO
import org.seekloud.yubel.paperClient.Protocol._
import org.seekloud.byteobject.MiddleBufferInJvm
import org.seekloud.essf.io.{FrameData, FrameInputStream, FrameOutputStream}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import org.slf4j.LoggerFactory
import org.seekloud.yubel.Boot.executor
import org.seekloud.yubel.ptcl.RoomApiProtocol
import org.seekloud.yubel.ptcl.RoomApiProtocol.{CommonRsp, ErrorRsp, RecordFrameInfo}
import org.seekloud.utils.essf.RecallGame._

/**
  * Created by dry on 2018/10/19.
  */
object GameReplay {
  sealed trait Command

  private val log = LoggerFactory.getLogger(this.getClass)

  private val waitTime=10.minutes

  case class TimeOut(msg: String) extends Command

  private final case object BehaviorChangeKey
  private final case object BehaviorWaitKey
  private final case object GameLoopKey
  case object GameLoop extends Command

  case class Left() extends Command
  case class GetRecordFrame(playerId: String, replyTo: ActorRef[CommonRsp]) extends Command
  case class StopReplay() extends Command

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String, behavior: Behavior[Command], durationOpt: Option[FiniteDuration] = None,timeOut: TimeOut  = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer:TimerScheduler[Command]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey, timeOut, _))
    stashBuffer.unstashAll(ctx, behavior)
  }

  case class InitReplay(subscriber: ActorRef[WsSourceProtocol.WsMsgSource], userId:String, f:Int) extends Command


  def create(recordId:Long, playerId: String): Behavior[Command] = {
    Behaviors.setup[Command]{ ctx=>
      log.info(s"${ctx.self.path} is starting..")
      implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
      implicit val sendBuffer = new MiddleBufferInJvm(81920)
      Behaviors.withTimers[Command] { implicit timer =>
        RecordDAO.getRecordById(recordId).map {
          case Some(r)=>
            try{
//            log.debug(s"game path ${r.filePath}")
              val replay=initInput("../backend/" + r.filePath) //for reStart
//            val replay=initInput(r.filePath) //for nohup
              val info=replay.init()

//              println(s"test2:${metaDataDecode(info.simulatorMetadata).right.get}")
//              println(s"test3:${replay.getMutableInfo(AppSettings.essfMapKeyName)}")
              log.debug(s"userInfo:${userMapDecode(replay.getMutableInfo(AppSettings.essfMapKeyName).getOrElse(Array[Byte]())).right.get.m}")
              val metaData = metaDataDecode(info.simulatorMetadata).right.get
              val frameRate = metaData.mode match {
                case 2 => frameRate2
                case _ => frameRate1
              }
              ctx.self ! SwitchBehavior("work",
                work(
                  replay,
                  metaData,
                  info.frameCount,
                  userMapDecode(replay.getMutableInfo(AppSettings.essfMapKeyName).getOrElse(Array[Byte]())).right.get.m,
                  frameRate = frameRate
                ))
            }catch {
              case e:Throwable=>
                log.error("error---"+e.getMessage)
                ctx.self ! SwitchBehavior("initError",initError)
            }
          case None=>
            log.debug(s"record--$recordId didn't exist!!")
            ctx.self ! SwitchBehavior("initError",initError)
        }
        switchBehavior(ctx,"busy",busy())
      }
    }
  }

  def work(fileReader:FrameInputStream,
           metaData:GameInformation,
           frameCount: Int,
           userMap:List[((Protocol.UserBaseInfo, List[Protocol.UserJoinLeft]))],
           userOpt:Option[ActorRef[WsSourceProtocol.WsMsgSource]]=None,
           playedId: String = "",
           frameRate: Int
          )(
            implicit stashBuffer:StashBuffer[Command],
            timer:TimerScheduler[Command],
            sendBuffer: MiddleBufferInJvm
          ):Behavior[Command]={
    Behaviors.receive[Command]{(ctx,msg)=>
      msg match {
        case msg:InitReplay=>
          log.info("start new replay!")
          print(s"msg.userId:::::${msg.userId}")
          timer.cancel(GameLoopKey)
          timer.cancel(BehaviorWaitKey)
          userMap.find(_._1.id == msg.userId) match {
            case Some(u)=>
              //todo dispatch gameInformation
              dispatchTo(msg.subscriber, Protocol.Id(msg.userId))
              dispatchTo(msg.subscriber, Protocol.Mode(metaData.mode))

              log.info(s" set replay from frame=${msg.f}")
              log.debug(s"get snapshot index::${fileReader.getSnapshotIndexes}")
              val nearSnapshotIndex = fileReader.gotoSnapshot(msg.f)
//              val indexes = fileReader.getSnapshotIndexes.map(_._1)
//              val nearSnapshotIndex = indexes.filter(f => f <= msg.f).max
              log.debug(s"nearSnapshotIndex: $nearSnapshotIndex")
              //              fileReader.reset()
              //              for(i <- 1 to msg.f){
              //                if(fileReader.hasMoreFrame){
              //                  fileReader.readFrame()
              //                }
              //              }
//              fileReader.gotoSnapshot(msg.f)
              log.info(s"replay from frame=${fileReader.getFramePosition}")
              log.debug(s"start loading ======")
              dispatchTo(msg.subscriber, Protocol.StartLoading(nearSnapshotIndex))

              for(i <- 0 until (msg.f - fileReader.getFramePosition)){
                if(fileReader.hasMoreFrame){
                  fileReader.readFrame().foreach { f => dispatchByteTo(msg.subscriber, f)
                  }
                }else{
                  log.debug(s"${ctx.self.path} file reader has no frame, reply finish")
                  dispatchTo(msg.subscriber, Protocol.ReplayFinish(msg.userId))
                }
              }
              log.debug(s"start replay ======")
              dispatchTo(msg.subscriber, Protocol.StartReplay(nearSnapshotIndex, fileReader.getFramePosition))

              if(fileReader.hasMoreFrame){
                timer.startPeriodicTimer(GameLoopKey, GameLoop, frameRate.millis)
                work(fileReader,metaData,frameCount,userMap,Some(msg.subscriber), msg.userId, frameRate)
              }else{
                timer.startSingleTimer(BehaviorWaitKey,TimeOut("wait time out"),waitTime)
                Behaviors.same
              }
            case None=>
              timer.startSingleTimer(BehaviorWaitKey,TimeOut("wait time out"),waitTime)
              Behaviors.same
          }


        case GameLoop=>
          if(fileReader.hasMoreFrame){
//            val joinLeftInfo = userMap.filter(_._1.id == playedId).head._2
//            val leftInfo = joinLeftInfo.map(_.leftFrame)
//            val joinInfo = joinLeftInfo.map(_.joinFrame).sorted
//            if (leftInfo.contains(fileReader.getFramePosition)) {
//              fileReader.gotoSnapshot(joinInfo.filter(f => f.toInt > fileReader.getFramePosition).head.toInt)
//            }
            userOpt.foreach(u=>
              fileReader.readFrame().foreach { f =>
                dispatchByteTo(u, f)
              }
            )

            Behaviors.same
          }else{
            log.debug(s"has not more frame")
            timer.cancel(GameLoopKey)
            userOpt.foreach(u => dispatchTo(u, Protocol.ReplayFinish("userId")))

            timer.startSingleTimer(BehaviorWaitKey,TimeOut("wait time out"),waitTime)
            Behaviors.same
          }

        case GetRecordFrame(playerId, replyTo) =>
//          log.info(s"game replay got $msg")
          replyTo ! RoomApiProtocol.RecordFrameRsp(RoomApiProtocol.RecordFrameInfo(fileReader.getFramePosition, frameCount, frameRate))
          Behaviors.same

        case StopReplay() =>
          Behaviors.stopped


//        case msg:GetRecordFrameMsg=>
//          msg.replyTo ! GetRecordFrameRsp(RecordFrameInfo(fileReader.getFramePosition))
//          Behaviors.same
//
//        case msg:GetUserInRecordMsg=>
//          val data=userMap.map{r=>PlayerInfo(r._1.userId,r._1.name)}
//          msg.replyTo ! GetUserInRecordRsp(PlayerList(data))
//          Behaviors.same

        case msg:TimeOut=>
          Behaviors.stopped

        case unKnowMsg =>
          stashBuffer.stash(unKnowMsg)
          Behavior.same
      }
    }
  }

  private def initError(
                         implicit sendBuffer: MiddleBufferInJvm
                       ):Behavior[Command]={
    Behaviors.receive[Command]{(ctx,msg)=>
      msg match {
        case msg:InitReplay =>
          log.debug(s"游戏文件不存在或已损坏")
          dispatchTo(msg.subscriber, InitReplayError("游戏文件不存在或者已损坏！！"))
          Behaviors.stopped

        case msg:GetRecordFrame=>
          msg.replyTo ! ErrorRsp(10001,"init error")
          Behaviors.stopped

        case msg=>
          log.debug(s"unknown message:$msg")
          Behaviors.stopped
      }
    }
  }



  import org.seekloud.byteobject.ByteObject._
  def dispatchTo(subscriber: ActorRef[WsSourceProtocol.WsMsgSource], msg: Protocol.GameMessage)(implicit sendBuffer: MiddleBufferInJvm)= {
    //    subscriber ! ReplayFrameData(msg.asInstanceOf[TankGameEvent.WsMsgServer].fillMiddleBuffer(sendBuffer).result())
    subscriber ! msg
  }

  def dispatchByteTo(subscriber: ActorRef[WsSourceProtocol.WsMsgSource], msg:FrameData)(implicit sendBuffer: MiddleBufferInJvm) = {
    //    subscriber ! ReplayFrameData(replayEventDecode(msg.eventsData).fillMiddleBuffer(sendBuffer).result())
    //    msg.stateData.foreach(s=>subscriber ! ReplayFrameData(replayStateDecode(s).fillMiddleBuffer(sendBuffer).result()))
    val events = replayEventDecode(msg.eventsData)
    val state = if(msg.stateData.isEmpty) None else Some(replayStateDecode(msg.stateData.get))
    subscriber ! ReplayFrameData(msg.frameIndex, events, state)
  }

  private def busy()(
    implicit stashBuffer:StashBuffer[Command],
    timer:TimerScheduler[Command]
  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case SwitchBehavior(name, behavior,durationOpt,timeOut) =>
          log.debug(s"switchBehavior")
          switchBehavior(ctx,name,behavior,durationOpt,timeOut)

        case TimeOut(m) =>
          log.debug(s"${ctx.self.path} is time out when busy,msg=${m}")
          Behaviors.stopped

        case unknowMsg =>
          stashBuffer.stash(unknowMsg)
          Behavior.same
      }
    }


}
