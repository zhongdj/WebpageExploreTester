package net.imadz.web.explorer

import java.net.ConnectException
import java.util.concurrent.{Executors, ThreadFactory, TimeoutException}

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.HttpErrorRecorder.HttpError
import net.imadz.web.explorer.ImgDownloadLead.ImgDownloadRequest
import net.imadz.web.explorer.ParserLead.ParseRequest

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
 * Created by geek on 8/20/14.
 */
class HttpUrlGetter(httpRequest: HttpRequest) extends Actor with ActorLogging {

  val downloadDirect = false
  val HANDSHAKE_NOT_COMPLETE: Int = 900

  self ! httpRequest

  def client = AsyncWebClient

  import net.imadz.web.explorer.HttpUrlGetter.exec

  override def receive: Receive = LoggingReceive {
    case p@PageRequest(_, url, name, pre, _) =>

      val headers: Map[String, String] = httpRequest.headers

      client.get(headers)(encodeUrl(url))(context.parent, p) onComplete {
        case Success(body) =>
          context.actorSelection(ParserLead.path) ! ParseRequest(body, p)
          context.stop(self)
        case Failure(BadStatus(code)) =>
          HttpUrlGetter.reporterCount += 1
          context.actorSelection(HttpErrorRecorder.path) ! HttpError(code, p)
          context.stop(self)
        case Failure(t) =>
          context.stop(self)
          //handleFailure(t, p)
      }

    case i@ImageRequest(_, url, name, pre, _) =>
      if (downloadDirect) {
        context.actorSelection(ImgDownloadLead.path) ! ImgDownloadRequest(url, pre)
        context.stop(self)
      } else {
        val headers: Map[String, String] = httpRequest.headers
        client.connectOnly(headers)(encodeUrl(url)) onComplete {
          case Success(x) =>
            log.info("image @ " + url + " is available.")
            context.actorSelection(ImgDownloadLead.path) ! ImgDownloadRequest(url, pre)
            context.stop(self)
          case Failure(BadStatus(code)) =>
            log.error("image @ " + url + " is unavailable.")
            context.actorSelection(HttpErrorRecorder.path) ! HttpError(code, i)
            context.stop(self)
          case Failure(t) =>
            context.stop(self)
          //handleFailure(t, i)
        }
      }
  }

  def encodeUrl(url: String): String = {
    url.replaceAll(" ", "%20")
  }

  def handleFailure(t: Throwable, i: HttpRequest): Unit = {
    //context.parent ! SlowDown
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
          context.stop(self)
        }
      case e: Throwable =>
        log.error(e, "Unhandled Exception")
        context.stop(self)
      case _ =>
        log.error(t, i.toString)
        context.stop(self)
    }
  }
}

object HttpUrlGetter {

  var reporterCount: Int = 0
  var parserCount: Int = 0
  implicit val exec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2, new ThreadFactory {
    var counter = 0;

    override def newThread(r: Runnable): Thread = {
      counter += 1
      new Thread(r, "HttpUrlGetter-onComplete-Pool-" + counter)
    }
  }))

  def propsOfPages(httpRequest: HttpRequest) = Props(classOf[HttpUrlGetter], httpRequest).withDispatcher("pages-dispatcher")

}

