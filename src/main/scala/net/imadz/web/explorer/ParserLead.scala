package net.imadz.web.explorer

import akka.actor._
import net.imadz.web.explorer.ParserLead.{PageEnd, PageParsed, ParseRequest}
import net.imadz.web.explorer.StateUpdate.{Parsing, ShuttingDown, Stopped}

import scala.concurrent.duration._

/**
 * Created by geek on 8/24/14.
 */
class ParserLead(urlbank: ActorRef, dispatcher: ActorRef, observer: Option[ActorRef]) extends Actor {

  var pageParsed = Map[String, Boolean]().withDefaultValue(false)
  var urlParserMap = Map[String, Set[String]]().withDefaultValue(Set.empty[String])
  var parserUrlMap = Map[String, String]().withDefaultValue("")

  override def receive: Receive = {
    case ParseRequest(body, page) =>
      ParserLead.parserCount += 1
      val parser = context.actorOf(HttpLinkParser.props(body, page, urlbank, observer), "HttpLinkParser-" + ParserLead.parserCount)
      context.watch(parser)

      urlParserMap += (page.url -> (urlParserMap(page.url) + parser.path.toString))
      parserUrlMap += (parser.path.toString -> page.url)

      notifyParsing
    case Terminated(parser) =>
      val url = parserUrlMap(parser.path.toString)

      urlParserMap = urlParserMap.updated(url, urlParserMap(url) - parser.path.toString)
      parserUrlMap -= parser.path.toString

      if (pageParsed(url) && urlParserMap(url).isEmpty) dispatcher ! PageParsed(url)
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
    case PageEnd(pageUrl) =>
      pageParsed = pageParsed.updated(pageUrl, true)
      if (urlParserMap(pageUrl).isEmpty) dispatcher ! PageParsed(pageUrl)
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

  case class PageEnd(pageUrl: String)

  case class PageParsed(pageUrl: String)

  def apply(context: ActorContext, urlBank: ActorRef, dispatcher: ActorRef, observer: Option[ActorRef]) = context.actorOf(props(urlBank, dispatcher, observer), name)

  def props(urlBank: ActorRef, dispatcher: ActorRef, observer: Option[ActorRef]) = Props(classOf[ParserLead], urlBank, dispatcher, observer)
}
