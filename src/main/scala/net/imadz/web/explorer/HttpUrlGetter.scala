package net.imadz.web.explorer

import akka.actor.{ActorRef, Props, Actor}
import akka.actor.Actor.Receive
import akka.pattern.pipe

import scala.concurrent.Future
import scala.util.{Success, Failure}

/**
 * Created by geek on 8/20/14.
 */
class HttpUrlGetter(httpRequest: HttpRequest) extends Actor {

  self ! httpRequest

  def client: WebClient = AsyncWebClient

  override def receive: Receive = {
    case p@PageRequest(_, url, pre, _) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val headers: Map[String, String] = httpRequest.headers
      client.get(headers)(url) onComplete {
        case Success(body) => context.actorOf(HttpLinkParser.props(body, p, context.parent))
        case Failure(BadStatus(code)) => context.actorOf(HttpErrorRecorder.props(code, p))
        case Failure(t) => self ! p
      }

    case i@ImageRequest(_, url, pre, _) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val headers: Map[String, String] = httpRequest.headers
      client.get(headers)(url) onComplete {
        case Success(ignore) => None
        case Failure(BadStatus(code)) => context.actorOf(HttpErrorRecorder.props(code, i))
        case Failure(t) => self ! i
      }
  }
}

object HttpUrlGetter {

  def props(httpRequest: HttpRequest) = Props(classOf[HttpUrlGetter], httpRequest)

}

