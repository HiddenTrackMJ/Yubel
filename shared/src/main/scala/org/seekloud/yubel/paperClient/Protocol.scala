package org.seekloud.yubel.paperClient

/**
  * User: Taoz
  * Date: 8/29/2016
  * Time: 9:40 PM
  */
object Protocol {

  sealed trait GameMessage extends WsSourceProtocol.WsMsgSource

  case class SyncFrame(
                        frameCount: Int
                      ) extends GameMessage

  case class NewFieldInfo(
                           frameCount: Int,
                           fieldDetails: List[FieldByColumnCondensed]
                         ) extends GameMessage

  case class Data4TotalSync(
                             frameCount: Int,
                             snakes: List[SkDt],
                             bodyDetails: List[BodyBaseInfo],
                             fieldDetails: List[FieldByColumn]
                           ) extends GameMessage

  case class AllData(
                      frameCount: Int,
                      bricks: Map[Point,Brick],
                      boards: Map[String,Board],
                      balls: Map[String,Ball]
                           ) extends GameMessage

  case class DataExceptBrick(
                             frameCount: Int,
                             boards: Map[String,Board],
                             balls: Map[String,Ball],
                             score: Map[String,Score]
                           ) extends GameMessage

  case class ScoreData(
                  score: Map[String,Score]
                  ) extends GameMessage

  case class GameWin(
                    score: Score
                    ) extends  GameMessage
//  case class Data4TotalSyncCondensed(
//                             frameCount: Int,
//                             snakes: List[SkDt],
//                             bodyDetails: List[BodyBaseInfoCondensed],
//                             fieldDetails: List[FieldByColumnCondensed]
//                           ) extends GameMessage


  case class KilledSkData(
                           killedSkInfo: List[KilledSkDt]
                         )

  case class Point4Trans(x: Short, y: Short)

  case class BodyBaseInfo(
                           uid: String,
                           turn: TurnInfo
                         )

//  case class BodyBaseInfoCondensed(
//                           uid: Byte,
//                           turn: TurnInfoCondensed
//                         )

  case class TurnInfo(
                       turnPoint: List[Point4Trans],
                       pointOnField: List[(Point4Trans, String)]
                     ) // (拐点顺序list， 占着别人领域的身体点)

//  case class TurnInfoCondensed(
//                       turnPoint: List[Point4Trans],
//                       pointOnField: List[(Point4Trans, Byte)]
//                     )

  case class FieldByColumn(
                            uid: String,
                            scanField: List[ScanByColumn]
                          )

  case class FieldByColumnCondensed(
                            uid: Byte,
                            scanField: List[ScanByColumn]
                          )

  case class ScanByColumn(
                           y: List[(Short, Short)],
                           x: List[(Short, Short)]
                         )

  case class TextMsg(
                      msg: String
                    ) extends GameMessage

  case class Id(id: String) extends GameMessage

  case class RoomId(roomId: String) extends GameMessage

  case class Id4Watcher(id: String, watcher: String) extends GameMessage

  case class StartWatching(mode: Int, img: Int) extends GameMessage

  case class Mode(mode: Int) extends GameMessage

  case class StartLoading(frame: Int) extends GameMessage

  case class StartReplay(firstSnapshotFrame: Int, firstReplayFrame: Int) extends GameMessage

  case class DeadPage(kill: Short, area: Short, playTime: Short) extends GameMessage

  case class DeadBoard(frameCount: Int, score: Long, playTime: Short) extends GameMessage

  case class OthersDead(frameCount: Int,id : List[String]) extends GameMessage

  case class NewBoard(allNew: DataExceptBrick) extends GameMessage

  case class UserDeadMsg(frame: Int, deadInfo: List[BaseDeadInfo]) extends GameMessage

//  case class BaseDeadInfo(id: String, name: String, killerName: Option[String])
  case class BaseDeadInfo(carnieId: Byte, killerId: Option[Byte])

  case class UserDead(frame: Int, id: String, name: String, killerId: Option[String]) extends GameMessage with GameEvent

  case class UserLeft(id: String) extends GameMessage

  case class InitReplayError(info: String) extends GameMessage

  case class BoardAction(id: Byte, keyCode: Byte, frame: Int, actionId: Int, typ: Int) extends GameMessage

  case class OtherAction(id: Byte, keyCode: Byte, frame: Int, typ: Int) extends GameMessage

  case object CloseWs extends GameMessage

  case class ReplayFinish(id: String) extends GameMessage

  case class Ranks(currentRank: List[Sc], personalScore: Option[Sc], personalRank: Option[Byte], currentNum: Byte) extends GameMessage

  case object ReStartGame extends GameMessage

  case class SomeOneWin(winnerName: String) extends GameMessage with GameEvent

  case class WinData(winnerScore: Short, yourScore: Option[Short], winnerName: String) extends GameMessage with GameEvent

  //  case class SomeWin(winnerName: String,finalData:Data4TotalSync) extends GameMessage with GameEvent

  case class ReceivePingPacket(pingId: Short) extends GameMessage

  case class WinnerBestScore(Score: Short) extends GameMessage

  sealed trait WsSendMsg

  case object WsSendComplete extends WsSendMsg

  case class WsSendFailed(ex: Throwable) extends WsSendMsg

  sealed trait UserAction extends WsSendMsg

  case class Key(keyCode: Byte, frameCount: Int, actionId: Int) extends UserAction

  case class Keys(keyCode: Byte, frameCount: Int, actionId: Int, typ: Int) extends UserAction

  case class TextInfo(msg: String) extends UserAction

  case class SendPingPacket(actionId: Short) extends UserAction

  case object NeedToSync extends UserAction

  case object PressSpace extends UserAction

  //essf
  sealed trait GameEvent extends GameMessage

  case class NewSnakeInfo(
//                           frameCount: Int,
                           snake: List[SkDt],
                           filedDetails: List[FieldByColumnCondensed]
                         ) extends GameEvent

  case class NewData(frameCount: Int,
                     newSnakes: Option[NewSnakeInfo],
                     newField: Option[List[FieldByColumnCondensed]]
                    ) extends GameEvent

  case class InitActions(actions: List[OtherAction]) extends GameEvent


  case class JoinEvent(id: String, name: String) extends GameEvent

  case class LeftEvent(id: String, nickName: String) extends GameEvent

  case class SpaceEvent(id: String) extends GameEvent

  case class DirectionEvent(id: String, keyCode: Int) extends GameEvent

  case class EncloseEvent(fieldDetails: List[FieldByColumn]) extends GameEvent

  case class EventData(events: List[GameEvent]) extends GameEvent

  case class RankEvent(rank: List[Sc]) extends GameEvent

  case class Snapshot(snakes: List[SkDt], bodyDetails: List[BodyBaseInfo], fieldDetails: List[FieldByColumn]) extends GameEvent

  case class DecodeError() extends GameEvent

  case class GameInformation(roomId: Int, startTime: Long, index: Int, initFrame: Long, mode: Int)

  case class UserJoinLeft(joinFrame: Long, leftFrame: Long)

  case class UserBaseInfo(id: String, name: String)

  case class EssfMapInfo(m: List[(UserBaseInfo, List[UserJoinLeft])])

  //for replay
  //  sealed trait ReplayMessage extends WsSourceProtocol.WsMsgSource

  /**
    * replay-frame-msg*/
  //  case class ReplayFrameData(frameIndex: Int, eventsData: Array[Byte], stateData: Option[Array[Byte]]) extends GameMessage
  case class ReplayFrameData(frameIndex: Int, eventsData: GameEvent, stateData: Option[GameEvent]) extends GameMessage

  val frameRate1 = 75 //normal-mode 150
  val frameRate2 = 75 //doubleSpeed-mode
  val maxContainableAction = 3

  val brickWidth = 4
  val brickHeight = 2
  val ballRadius = 0.5
  private var BoardWidth = 10
  val boardHeight = 1
  val scorePerBrick = 100

  def getBoardWidth: Int = BoardWidth

  def setBoardWidth(w: Int): Unit = {
    BoardWidth = w
  }

}
