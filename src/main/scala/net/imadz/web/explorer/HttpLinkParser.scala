package net.imadz.web.explorer

import akka.actor.{Props, Actor, ActorLogging, ActorRef}
import akka.event.LoggingReceive
import net.imadz.web.explorer.utils.LinkUtils
import scala.util.matching.Regex

/**
 * Created by geek on 8/20/14.
 */
class HttpLinkParser(body: String, httpRequest: PageRequest, dispatcher: ActorRef) extends Actor with ActorLogging {

  self ! body

  override def receive: Receive = LoggingReceive {
    case body: String =>
      parse(body) { request =>
        dispatcher ! request
      }
      context.stop(self)
  }

  private def parse(body: String)(dispatch: HttpRequest => Unit) = {
    try {
      LinkUtils.findLinks(body, httpRequest) foreach (newLink => dispatch(newLink))
    } catch {
      case t =>
        context.stop(self)
        throw t
    }
  }
}


object HttpLinkParser {

  def props(body: String, httpRequest: PageRequest, dispatcher: ActorRef) = Props(classOf[HttpLinkParser], body, httpRequest, dispatcher).withDispatcher("parser-dispatcher")

}

