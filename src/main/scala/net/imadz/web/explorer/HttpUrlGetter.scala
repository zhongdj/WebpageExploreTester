package net.imadz.web.explorer

import java.util.concurrent.{Executors, ThreadFactory}

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.HttpErrorRecorder.HttpErrorRequest
import net.imadz.web.explorer.ImgDownloadLead.ImgDownloadRequest

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
 * Created by geek on 8/20/14.
 */
class HttpUrlGetter(httpRequest: HttpRequest, urlBank: ActorRef) extends Actor with ActorLogging {

  val downloadDirect = false
  val HANDSHAKE_NOT_COMPLETE: Int = 900

  self ! httpRequest

  def client = AsyncWebClient

  import net.imadz.web.explorer.HttpUrlGetter.exec

  override def receive: Receive = LoggingReceive {
    case p@PageRequest(_, url, name, pre, _) =>
      val anchorFSM = context.actorOf(AnchorFSM.props(p))
      val imgFSM = context.actorOf(ImgFSM.props(p))
      context.watch(anchorFSM)
      context.watch(imgFSM)

      val headers: Map[String, String] = httpRequest.headers

      def shutdownChildren {
        anchorFSM ! Shutdown
        imgFSM ! Shutdown
      }
      client.get(headers)(encodeUrl(url))(urlBank, p, anchorFSM, imgFSM) onComplete {
        case Success(body) =>
          if (null != context) {
            shutdownChildren
          }
        case Failure(BadStatus(code)) =>
          HttpUrlGetter.reporterCount += 1
          context.actorSelection(HttpErrorRecorder.path) ! HttpErrorRequest(code, p)
          shutdownChildren
        case Failure(t) =>
          shutdownChildren
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
            context.actorSelection(HttpErrorRecorder.path) ! HttpErrorRequest(code, i)
            context.stop(self)
          case Failure(t) =>
            context.stop(self)
        }
      }
    case Terminated(child) => {
      if (context.children.isEmpty) {
        context.stop(self)
      }
    }
  }

  def encodeUrl(url: String): String = {
    url.replaceAll(" ", "%20")
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

  def propsOfPages(httpRequest: HttpRequest, urlBank: ActorRef) = Props(classOf[HttpUrlGetter], httpRequest, urlBank).withDispatcher("pages-dispatcher")

}

