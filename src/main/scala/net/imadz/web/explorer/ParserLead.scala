package net.imadz.web.explorer

import akka.actor.{Actor, ActorRef, ReceiveTimeout}
import net.imadz.web.explorer.ParserLead.ParseRequest

import scala.concurrent.duration._

/**
 * Created by geek on 8/24/14.
 */
class ParserLead(urlbank: ActorRef) extends Actor {

  override def receive: Receive = {
    case ParseRequest(body, page) =>
      ParserLead.parserCount += 1
      context.actorOf(HttpLinkParser.props(body, page, urlbank), "HttpLinkParser-" + ParserLead.parserCount)
    case Shutdown if context.children.isEmpty => context stop self
    case Shutdown => context.setReceiveTimeout(10 seconds)
    case ReceiveTimeout => context stop self

  }

}

object ParserLead {

  var parserCount: Int = 0
  val name = "ParserLead"
  val path = Main.path + name

  case class ParseRequest(body: String, page: PageRequest)

}
