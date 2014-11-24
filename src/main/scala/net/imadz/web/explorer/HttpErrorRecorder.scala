package net.imadz.web.explorer

import java.io.{File, PrintWriter}

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.HttpErrorRecorder.HttpErrorRequest
import net.imadz.web.explorer.StateUpdate.{HttpErrorFound, ShuttingDown, Stopped}
import net.imadz.web.explorer.utils.HttpResponseUtil

import scala.concurrent.duration._
import scala.io.Source

/**
 * Created by geek on 8/20/14.
 */
class HttpErrorRecorder(errorHandler: Option[ActorRef], observer: Option[ActorRef]) extends Actor with ActorLogging {
  import HttpErrorProtocol.HttpError

  //TODO 408 Error

  private def notifyHttpError: Receive = {
    case error: HttpErrorRequest =>
      for (handler <- errorHandler) yield handler ! HttpError(error.responseCode, error.httpRequest)
      for (listener <- observer) yield listener ! HttpErrorFound(HttpError(error.responseCode, error.httpRequest))
  }

  override def receive: Receive = notifyHttpError orElse LoggingReceive {
    case Shutdown =>
      for (listener <- observer) yield listener ! ShuttingDown(self)
      context become shuttingDown
      context.setReceiveTimeout(10 seconds)
  }

  private def shuttingDown: Receive = notifyHttpError orElse LoggingReceive {
    case ReceiveTimeout =>
      for (listener <- observer) yield listener ! Stopped(self)
      context stop self
  }
}

object HttpErrorRecorder {

  def apply(context: ActorContext, errorHandler: Option[ActorRef], observer: Option[ActorRef]) = context.actorOf(props(errorHandler, observer), name)

  def props(errorHandler: Option[ActorRef], observer: Option[ActorRef]) = Props(classOf[HttpErrorRecorder], errorHandler, observer)

  case class HttpErrorRequest(responseCode: Int, httpRequest: HttpRequest)

  val name = "HttpErrorRecorder"
  val path = Main.path + name
}

object HttpErrorProtocol {

  case class HttpError(responseCode: Int, findInUrl: String, targetUrl: String, steps: List[NavigateSegment], raw: HttpRequest)

  object HttpError {

    private def stepsFrom(request: HttpRequest): List[NavigateSegment] = request match {
      case PageRequest(_, url, _, None, _) => List[NavigateSegment](LandingPage(url))
      case PageRequest(_, url, name, Some(previousPageRequest), _) => stepsFrom(previousPageRequest) :+ Step(name, url)
      case ImageRequest(_, url, _, pageRequest, _) => stepsFrom(pageRequest) :+ Step("Image", url)
    }

    def apply(responseCode: Int, httpRequest: HttpRequest): HttpError = httpRequest.previousRequest match {
      case Some(pre) => new HttpError(responseCode, pre.url, httpRequest.url, stepsFrom(httpRequest), httpRequest)
      case None => new HttpError(responseCode, "", httpRequest.url, stepsFrom(httpRequest), httpRequest)
    }
  }

  sealed abstract class NavigateSegment(url: String)

  case class LandingPage(url: String) extends NavigateSegment(url)

  case class Step(linkName: String, targetUrl: String) extends NavigateSegment(targetUrl)

}
