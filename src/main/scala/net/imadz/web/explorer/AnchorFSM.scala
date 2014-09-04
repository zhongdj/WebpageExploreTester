package net.imadz.web.explorer

import akka.actor._
import AnchorFSM._
import net.imadz.web.explorer.ParserLead.ParseRequest

/**
 * Created by Scala on 14-9-4.
 */
class AnchorFSM(p: PageRequest) extends Actor with FSM[State, Data] with ActorLogging {
  startWith(WaitAnchorStart, Empty)
  val parserLead: ActorSelection = context.actorSelection(ParserLead.path)

  assert(null != parserLead)

  when(WaitAnchorStart) {
    case Event(NewLine(rawLine), Empty) => {
      try {
        val newLine = rawLine.trim
        if (newLine.contains("<a")) {
          val result = newLine.split("</a>")
          if (result.length == 1) goto(WaitAnchorEnd) using Buffer(lines = List(result(0)))
          else {
            (0 until (result.length - 1)) map { x =>
              val body = result(x) + "</a>"
              parserLead ! ParseRequest(body, p)
            }
            if (result(result.length - 1).contains("<a")) {
              goto(WaitAnchorEnd) using Buffer(lines = List(result(result.length - 1)))
            } else {
              stay using Empty
            }
          }
        } else {
          stay using Empty
        }
      } catch {
        case e: Exception => log.error(e, e.getMessage)
          stay
      }
    }
    case Event(Shutdown, _) => {
      context stop self
      stay
    }
  }

  when(WaitAnchorEnd) {
    case Event(NewLine(rawLine), Buffer(data)) => {
      try {
        val newLine = rawLine.trim
        val result = newLine.split("</a>")
        if (result.length == 0) {
          val last = data :+ "</a>"

          parserLead ! ParseRequest(last mkString, p)
          goto(WaitAnchorStart) using Empty
        } else if (result.length == 1) {
          stay using Buffer(lines = data :+ result(0))
        } else {
          val last = data :+ result(0) :+ "</a>"
          parserLead ! ParseRequest(last mkString, p)

          1 until (result.length - 1) map { x =>
            parserLead ! ParseRequest(result(x) + "</a>", p)
          }

          if (result(result.length - 1).contains("<a")) {
            stay using Buffer(lines = List(result(result.length - 1)))
          } else {
            goto(WaitAnchorStart) using Empty
          }
        }
      } catch {
        case e: Exception => log.error(e, e.getMessage)
          stay
      }
    }
    case Event(Shutdown, _) => {
      context stop self
      stay
    }
  }
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
