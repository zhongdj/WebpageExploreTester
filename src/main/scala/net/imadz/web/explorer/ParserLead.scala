package net.imadz.web.explorer

import akka.actor._
import net.imadz.web.explorer.ParserLead.ParseRequest
import net.imadz.web.explorer.StateUpdate.{Parsing, ShuttingDown, Stopped}

import scala.concurrent.duration._

/**
 * Created by geek on 8/24/14.
 */
class ParserLead(urlbank: ActorRef, observer: Option[ActorRef]) extends Actor {

  override def receive: Receive = {
    case ParseRequest(body, page) =>
      ParserLead.parserCount += 1
      val parser = context.actorOf(HttpLinkParser.props(body, page, urlbank, observer), "HttpLinkParser-" + ParserLead.parserCount)
      context.watch(parser)
      notifyParsing
    case Terminated(_) =>
      notifyParsing
    case Shutdown if context.children.isEmpty =>
      notifyStopped
      context stop self
    case Shutdown =>
      notifyShuttingDown
      context.setReceiveTimeout(10 seconds)
    case ReceiveTimeout =>
      notifyStopped
      context stop self
  }

  def notifyParsing = for (listener <- observer) yield listener ! Parsing(context.children.size)

  def notifyShuttingDown = for (listener <- observer) yield listener ! ShuttingDown(self)

  def notifyStopped = for (listener <- observer) yield listener ! Stopped(self)

}

object ParserLead {

  var parserCount: Int = 0
  val name = "ParserLead"
  val path = Main.path + name

  case class ParseRequest(body: String, page: PageRequest)

  def apply(context: ActorContext, urlBank: ActorRef, observer: Option[ActorRef]) = context.actorOf(props(urlBank, observer), name)

  def props(urlBank: ActorRef, observer: Option[ActorRef]) = Props(classOf[ParserLead], urlBank, observer)
}
