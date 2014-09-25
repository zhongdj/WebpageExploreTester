package net.imadz.web.explorer

import akka.actor._
import net.imadz.web.explorer.utils.LinkUtils

import scala.collection.immutable.ListSet
import scala.concurrent.duration._
import scala.concurrent.Future
import net.imadz.web.explorer.StateUpdate.UrlParsed

/**
 * Created by geek on 8/20/14.
 */
class HttpLinkParser(body: String, httpRequest: PageRequest, urlBank: ActorRef, observer: Option[ActorRef]) extends Actor with ActorLogging with Timeout {

  self ! body

  override def receive: Receive = {
    case body: String => resetTimeout(context) {
      implicit val exec = context.dispatcher
      if (body.length > 2500) log.debug("[HttpLinkParser] The body is too long, the body is : " + body)
      Future {
        parse(body) { request =>
        {
          //for (o <- observer) o ! UrlParsed(UrlParsedInfo(request.url, request.name, request.depth))
          urlBank ! UrlBank.Deposit(ListSet(request))
        }
        }
      } onComplete (_ => if (context != null) context.stop(self))
    }(30 seconds)
    case ReceiveTimeout =>
      log.warning("Parser cost too much time. Over 30 seconds, being Killed, content as following: ")
      log.warning(body)
      context.stop(self)
  }

  private def parse(body: String)(dispatch: HttpRequest => Unit) = {
    try {
      LinkUtils.findLinks(body, httpRequest, context.system.settings.config.getBoolean("imadz.web.explorer.downloadImage")) foreach (newLink => dispatch(newLink))
    } catch {
      case t: Throwable =>
        context.stop(self)
        throw t
    }
  }
}


object HttpLinkParser {

  def props(body: String, httpRequest: PageRequest, dispatcher: ActorRef, observer: Option[ActorRef]) = Props(classOf[HttpLinkParser], body, httpRequest, dispatcher, observer).withDispatcher("parser-dispatcher")

}

case class UrlParsedInfo(url: String, name: String, depth: Int)


