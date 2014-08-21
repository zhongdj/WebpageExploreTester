package net.imadz.web.explorer

import akka.actor.{ActorLogging, Props, Actor}
import akka.actor.Actor.Receive

/**
 * Created by geek on 8/20/14.
 */
class HttpErrorRecorder(responseCode: Int, httpRequest: HttpRequest) extends Actor  with ActorLogging {

  self ! httpRequest

  override def receive: Receive = {
    case p: PageRequest =>
      logPageError(p)
      context.stop(self)
    case i: ImageRequest =>
      logImageError(i)
      context.stop(self)
  }

  def logPageError(request: PageRequest): Unit = {
    log.error(request.toString)
  }

  def logImageError(request: ImageRequest): Unit = {
    log.error(request.toString)
  }
}

object HttpErrorRecorder {

  def props(responseCode: Int, httpRequest: HttpRequest) = Props(classOf[HttpErrorRecorder], responseCode, httpRequest)

  case class HttpError(responseCode: Int, httpRequest: HttpRequest)

}
