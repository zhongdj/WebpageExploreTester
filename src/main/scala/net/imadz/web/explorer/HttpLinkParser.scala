package net.imadz.web.explorer

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import net.imadz.web.explorer.utils.LinkUtils

/**
 * Created by geek on 8/20/14.
 */
class HttpLinkParser(body: String, httpRequest: PageRequest, dispatcher: ActorRef) extends Actor with ActorLogging {

  self ! body

  override def receive: Receive = {
    case body: String =>
      parse(body) { request =>
        dispatcher ! request
      }
      context.stop(self)
  }

  private def parse(body: String)(dispatch: HttpRequest => Unit) = {
    try {
      LinkUtils.findLinks(body, httpRequest, context.system.settings.config.getBoolean("imadz.web.explorer.downloadImage")) foreach (newLink => dispatch(newLink))
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

