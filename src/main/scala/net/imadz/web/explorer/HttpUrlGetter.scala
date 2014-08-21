package net.imadz.web.explorer

import java.net.ConnectException
import java.util.concurrent.TimeoutException

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.HttpErrorRecorder.HttpError

import scala.util.{Failure, Success}

/**
 * Created by geek on 8/20/14.
 */
class HttpUrlGetter(httpRequest: HttpRequest) extends Actor with ActorLogging {

  val HANDSHAKE_NOT_COMPLETE: Int = 900

  self ! httpRequest

  def client: WebClient = AsyncWebClient

  override def receive: Receive = LoggingReceive {
    case p@PageRequest(_, url, pre, _) =>
//      log.debug("========================Getter Receive Start==========================================")
//      log.debug(url)
//      log.debug("========================Getter Receive End  ==========================================")
      import scala.concurrent.ExecutionContext.Implicits.global


      val headers: Map[String, String] = httpRequest.headers
      client.get(headers)(url) onComplete {
        case Success(body) =>
          HttpUrlGetter.parserCount += 1
          val child = context.actorOf(HttpLinkParser.props(body, p, context.parent), "HttpLinkParser-" + HttpUrlGetter.parserCount)
          context.watch(child)
        case Failure(BadStatus(code)) =>
          HttpUrlGetter.reporterCount += 1
          context.actorSelection(HttpErrorRecorder.path) ! HttpError(code, p)
          context.stop(self)
        case Failure(t) => handleFailure(t, p)
      }

    case i@ImageRequest(_, url, pre, _) =>
//      log.debug("========================Getter Receive Start==========================================")
//      log.debug(url)
//      log.debug("========================Getter Receive End  ==========================================")
      import scala.concurrent.ExecutionContext.Implicits.global
      val headers: Map[String, String] = httpRequest.headers
      client.get(headers)(url) onComplete {
        case Success(ignore) =>
          None
        case Failure(BadStatus(code)) =>
          context.actorSelection(HttpErrorRecorder.path) ! HttpError(code, i)
          context.stop(self)
        case Failure(t) => handleFailure(t, i)
      }
    case Terminated(child) =>
      context.stop(self)

  }

  def handleFailure(t:Throwable, i: HttpRequest) {
    log.error(t, i.toString)
    t match {
      case e: TimeoutException =>
        self ! i
      case e: ConnectException =>
        if (e.getMessage.contains("Handshake did not complete")) {
          HttpUrlGetter.reporterCount += 1
          context.actorSelection(HttpErrorRecorder.path) ! HttpError(HANDSHAKE_NOT_COMPLETE, i)
          context.stop(self)
        } else if (e.getMessage.contains("Connection reset by peer")) {
          //todo waiting
          self ! i
        } else if (e.getMessage.contains("Operation timed out")) {
          self ! i
        } else {
          log.error(e, "Unhandled Exception")
        }
      case e: Throwable => log.error(e, "Unhandled Exception")
      case _ =>
    }
  }
}

object HttpUrlGetter {

  var reporterCount: Int = 0
  var parserCount: Int = 0

  def props(httpRequest: HttpRequest) = Props(classOf[HttpUrlGetter], httpRequest)

}

