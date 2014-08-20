package net.imadz.web.explorer

import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive

/**
 * Created by geek on 8/20/14.
 */
class HttpErrorRecorder(responseCode: Int, httpRequest: HttpRequest) extends Actor {

  self ! httpRequest

  override def receive: Receive = {
    case p: PageRequest =>
      logPageError(p)
    case i: ImageRequest =>
      logImageError(i)
  }

  def logPageError(request: PageRequest): Unit = {
    println(request)
  }

  def logImageError(request: ImageRequest): Unit = {
    println(request)
  }
}

object HttpErrorRecorder {

  def props(responseCode: Int, httpRequest: HttpRequest) = Props(classOf[HttpErrorRecorder], responseCode, httpRequest)

  case class HttpError(responseCode: Int, httpRequest: HttpRequest)

}
