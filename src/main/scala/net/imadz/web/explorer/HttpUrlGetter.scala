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
    case p@PageRequest(url, pre, _) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      client get url onComplete {
        case Success(body) => context.actorOf(HttpLinkParser.props(body, p, context.parent))
        case Failure(BadStatus(code)) => context.actorOf(HttpErrorRecorder.props(code, p))
      }

    case i@ImageRequest(url, pre, _) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      client get url onComplete {
        case Success(ignore) => None
        case Failure(BadStatus(code)) => context.actorOf(HttpErrorRecorder.props(code, i))
      }
  }
}

object HttpUrlGetter {

  def props(httpRequest: HttpRequest) = Props(classOf[HttpUrlGetter], httpRequest)

}

