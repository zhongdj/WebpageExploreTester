package net.imadz.web.explorer

import akka.actor._
import AnchorFSM._
import net.imadz.web.explorer.ParserLead.ParseRequest

/**
 * Created by Scala on 14-9-4.
 */
class AnchorFSM(p: PageRequest) extends Actor with FSM[State, Data] with ActorLogging {
  startWith(WaitAnchorStart, NoLineData)
  val parserLead: ActorSelection = context.actorSelection(ParserLead.path)

  assert(null != parserLead)

  when(WaitAnchorStart) {
    case Event(NewLine(rawLine), NoLineData) => {
      try {
        val newLine = rawLine.trim
        if (newLine.contains("<a")) {
          val result = newLine.split("</a>")
          if (result.length == 1) goto(WaitAnchorEnd) using WaitAnchorEndData(lines = List(result(0)))
          else {
            (0 until (result.length - 1)) map { x =>
              val body = result(x) + "</a>"
              parserLead ! ParseRequest(body, p)
            }
            if (result(result.length - 1).contains("<a")) {
              goto(WaitAnchorEnd) using WaitAnchorEndData(lines = List(result(result.length - 1)))
            } else {
              stay using NoLineData
            }
          }
        } else {
          stay using NoLineData
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
    case Event(NewLine(rawLine), WaitAnchorEndData(data)) => {
      try {
        val newLine = rawLine.trim
        val result = newLine.split("</a>")
        if  (result.length == 0) {
          val last = data mkString "" + "</a>"
          parserLead ! ParseRequest(last, p)
          goto(WaitAnchorStart) using NoLineData
        } else if (result.length == 1) {
          stay using WaitAnchorEndData(lines = data ::: List(result(0)))
        } else {
          var last = data mkString "" + result(0) + "</a>"
          parserLead ! ParseRequest(last, p)

          (1 until (result.length - 1)) map { x =>
            last = result(x) + "</a>"
            parserLead ! ParseRequest(last, p)
          }

          if (result(result.length - 1).contains("<a")) {
            stay using WaitAnchorEndData(lines = List(result(result.length - 1)))
          } else {
            goto(WaitAnchorStart) using NoLineData
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

  case object NoLineData extends Data

  case class WaitAnchorEndData(lines: List[String]) extends Data

  def props(p: PageRequest) = Props(classOf[AnchorFSM], p)
}
