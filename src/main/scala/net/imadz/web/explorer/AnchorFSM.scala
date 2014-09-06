package net.imadz.web.explorer

import akka.actor._
import AnchorFSM._
import net.imadz.web.explorer.ParserLead.ParseRequest

/**
 * Created by Scala on 14-9-4.
 */
class AnchorFSM(previousPageRequest: PageRequest) extends Actor with FSM[State, Data] with ActorLogging {
  startWith(WaitAnchorStart, Empty)
  val parserLead: ActorSelection = context.actorSelection(ParserLead.path)
  assert(null != parserLead)

  val ANCHOR_START: String = "<a "
  val ANCHOR_END: String = "</a>"
  val PLACE_HOLDER: String = "MADZ_PLACE_HOLDER"

  when(WaitAnchorStart) {
    case Event(NewLine(rawLine), Empty) =>
      val newLine = rawLine.trim
      if (!newLine.contains(ANCHOR_START)) {
        stay using Empty
      } else {
        val totalLines = (newLine + PLACE_HOLDER).trim.split(ANCHOR_END).toList
        linkLinesOf(totalLines) map toLink foreach {
          parserLead ! ParseRequest(_, previousPageRequest)
        }
        toState(totalLines)
      }
    case Event(Shutdown, _) =>
      stop
  }

  when(WaitAnchorEnd) {
    case Event(NewLine(rawLine), Buffer(cachedData)) =>
      val totalLines = (cachedData :+ rawLine :+ PLACE_HOLDER mkString).trim.split(ANCHOR_END).toList
      linkLinesOf(totalLines) map toLink foreach {
          parserLead ! ParseRequest(_, previousPageRequest)
      }
      toState(totalLines)
    case Event(Shutdown, _) =>
      stop
  }

  def linkLinesOf: List[String] => List[String] = {
    case Nil => List(ANCHOR_END)
    case x :: Nil => Nil
    case x :: xs => x :: linkLinesOf(xs)
  }

  def toState: List[String] => State = {
    case Nil => goto(WaitAnchorStart) using Empty
    case lastLine :: Nil if lastLine contains ANCHOR_START => goto(WaitAnchorEnd) using Buffer(lastLine.replace(PLACE_HOLDER, "") :: Nil)
    case lastLine :: Nil => goto(WaitAnchorStart) using Empty
    case x :: xs => toState(xs)
  }

  private def toLink: String => String = {
    case line: String => line + ANCHOR_END
  }

  initialize()
}

object AnchorFSM {

  final case class NewLine(newLine: String)

  sealed trait State

  case object WaitAnchorStart extends State

  case object WaitAnchorEnd extends State

  sealed trait Data

  case object Empty extends Data

  case class Buffer(lines: List[String]) extends Data

  def props(p: PageRequest) = Props(classOf[AnchorFSM], p)
}
