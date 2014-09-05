package net.imadz.web.explorer

import akka.actor._
import ImgFSM._
import net.imadz.web.explorer.ParserLead.ParseRequest
import net.imadz.web.explorer.ImgFSM.NewLine

/**
 * Created by Scala on 14-9-4.
 */
class ImgFSM(p: PageRequest) extends Actor with FSM[State, Data] with ActorLogging {
  startWith(WaitImgStart, Empty)
  val parserLead: ActorSelection = context.actorSelection(ParserLead.path)
  assert(null != parserLead)

  when(WaitImgStart) {
    case Event(NewLine(newLine), Empty) => {
      if (newLine.contains("<img")) {
        val result = newLine.split("<img")
        assert(result.length != 1)
        (1 to (result.length - 1)) map {
          x => parserLead ! ParseRequest("<img" + result(x), p)
        }
      }
      stay using Empty
    }

    case Event(Shutdown, _) => {
      context stop self
      stay
    }
  }

  initialize()
}


object ImgFSM {

  final case class NewLine(newLine: String)

  sealed trait State

  case object WaitImgStart extends State

  sealed trait Data

  case object Empty extends Data

  case class WaitImgEndData(lines: List[String]) extends Data

  def props(p: PageRequest) = Props(classOf[ImgFSM], p)
}