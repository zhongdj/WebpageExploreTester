package net.imadz.web.explorer

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.UrlBank.{Deposit, Payback, WithDraw}

import scala.concurrent.duration._
import scala.util.matching.Regex

/**
 * Created by geek on 8/20/14.
 */
class HttpRequestDispatcher(val headers: Map[String, String], val excludes: Set[String], val domainUrl: String, val domainName: String, val domainConstraints: Set[String], val maxDepth: Int) extends Actor with ActorLogging {

  private var pageGetterCount: Int = 0
  private var imageGetterCount: Int = 0
  private var visitedUrls = Set[String]()
  private val urlBank = context.actorOf(Props(classOf[UrlBank]), "UrlBank")
  private val getterMaxCount = context.system.settings.config.getInt("imadz.web.explorer.getterCount")
  urlBank ! WithDraw(getterMaxCount)

  self ! PageRequest(headers, domainUrl, domainName, None, 0)


  override def receive: Receive = processRequest

  private def resetTimeout(block: => Unit) = {
    context.setReceiveTimeout(Duration.Undefined)
    block
    context.setReceiveTimeout(90 seconds)
  }

  private def processRequest: Receive = LoggingReceive {
    case request: HttpRequest =>
      resetTimeout {
        onRequest(request) { request =>
          urlBank ! Deposit(List(request))
        }
      }
    case Payback(requests) =>
      resetTimeout {
        requests foreach {
          case request: PageRequest =>
            pageGetterCount += 1
            context.watch(context.actorOf(HttpUrlGetter.propsOfPages(request), "PageGetter-" + pageGetterCount))
          case request: ImageRequest =>
            imageGetterCount += 1
            context.watch(context.actorOf(HttpUrlGetter.propsOfPages(request), "ImageGetter-" + imageGetterCount))
        }
      }
    case ReceiveTimeout =>
      log.info("visitedUrls size = " + visitedUrls.size)
      context.stop(self)
    case Terminated(child) =>
      resetTimeout {
        urlBank ! WithDraw(1)
      }
  }

  private def onRequest(request: HttpRequest, checkUrl: Boolean = true)(func: HttpRequest => Unit) {
    val url = request.url
    if (!checkUrl) {
      func(request)
    } else if (needVisit(url, request.depth)) {
      //log.info(url)
      visitedUrls += url
      func(request)
    }
  }

  private def needVisit(url: String, depth: Int): Boolean = {
    !visitedUrls.contains(url) &&
      !exclude(url) &&
      !url.contains("#") &&
      url.length > 10 &&
      depth <= maxDepth &&
      obeyDomainConstraints(url)
  }

  private def exclude(url: String) = excludes.exists(url.startsWith(_))

  private def obeyDomainConstraints(url: String): Boolean = {
    val domainPrefixReg = new Regex( """(http|https)://.*?/""")
    val domainUrl: String = domainPrefixReg.findFirstIn(url).getOrElse(url)
    if (domainUrl != "") {
      domainConstraints.exists(keyword => domainUrl.contains(keyword))
    }
    else false
  }

}

object HttpRequestDispatcher {
  val name: String = "HttpRequestDispatcher"
  val path = Main.path + name

  def props(headers: Map[String, String], excludes: Set[String], initialUrl: String, initialName: String, domainConstraints: Set[String], maxDepth: Int) = Props(classOf[HttpRequestDispatcher], headers, excludes, initialUrl, initialName, domainConstraints, maxDepth)
}