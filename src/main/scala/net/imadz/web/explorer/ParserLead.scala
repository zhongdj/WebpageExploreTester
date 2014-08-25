package net.imadz.web.explorer

import akka.actor.{ActorRef, Actor}
import akka.actor.Actor.Receive
import net.imadz.web.explorer.ParserLead.ParseRequest

/**
 * Created by geek on 8/24/14.
 */
class ParserLead(dispatcher: ActorRef) extends Actor {

  override def receive: Receive = {
    case ParseRequest(body, page) =>
      ParserLead.parserCount += 1
      context.actorOf(HttpLinkParser.props(body, page, dispatcher), "HttpLinkParser-" + ParserLead.parserCount)
  }

}

object ParserLead {

  var parserCount: Int = 0
  val name = "ParserLead"
  val path = Main.path + name
  case class ParseRequest(body: String, page: PageRequest)

}
