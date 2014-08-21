package net.imadz.web.explorer

import akka.actor.{Terminated, ActorRef, Props, Actor}
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
        case Success(body) =>
          HttpUrlGetter.parserCount += 1
          val child = context.actorOf(HttpLinkParser.props(body, p, context.parent), "HttpLinkParser-" + HttpUrlGetter.parserCount)
          context.watch(child)
        case Failure(BadStatus(code)) =>
          HttpUrlGetter.reporterCount += 1
          val child = context.actorOf(HttpErrorRecorder.props(code, p), "HttpErrorRecorder-" + HttpUrlGetter.reporterCount)
          context.watch(child)
        case Failure(t) => self ! p
      }

    case i@ImageRequest(_, url, pre, _) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val headers: Map[String, String] = httpRequest.headers
      client.get(headers)(url) onComplete {
        case Success(ignore) =>
          None
        case Failure(BadStatus(code)) =>
          val child = context.actorOf(HttpErrorRecorder.props(code, i))
          context.watch(child)
        case Failure(t) => self ! i
      }
    case Terminated(child) =>
      context.stop(self)
  }
}

object HttpUrlGetter {

  var reporterCount : Int = 0
  var parserCount : Int = 0
  def props(httpRequest: HttpRequest) = Props(classOf[HttpUrlGetter], httpRequest)

}

