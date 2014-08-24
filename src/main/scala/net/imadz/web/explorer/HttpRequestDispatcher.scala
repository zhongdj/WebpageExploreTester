package net.imadz.web.explorer

import akka.actor.{Actor, ActorLogging, Props, ReceiveTimeout}
import akka.event.LoggingReceive

import scala.concurrent.duration._
import scala.util.matching.Regex

/**
 * Created by geek on 8/20/14.
 */
class HttpRequestDispatcher(val headers: Map[String, String], val excludes: Set[String], val domainUrl: String, val domainConstraints: Set[String], val maxDepth: Int) extends Actor with ActorLogging {

  var visitedUrls = Set[String]()
  var queue: List[HttpRequest] = Nil


  self ! PageRequest(headers, domainUrl, None, 0)

  def exclude(url: String) = excludes.exists(url.startsWith(_))

  var pageGetterCount: Int = 0
  var imageGetterCount: Int = 0

  private def onRequest(request: HttpRequest)(func: HttpRequest => Unit) {
    context.setReceiveTimeout(Duration.Undefined)
    val url = request.url
    if (needVisit(url, request.depth)) {
      //log.info(url)
      visitedUrls += url

      func(request)

      //log.info("cached url number: " + visitedUrls.size)
    }
    context.setReceiveTimeout(90 second)
  }

  override def receive: Receive = LoggingReceive {
    case p@PageRequest(_, rawUrl, previousRequest, depth) =>
      onRequest(p) { request =>
        if (obeyDomainConstraints(rawUrl)) {
          pageGetterCount += 1
          context.actorOf(HttpUrlGetter.propsOfPages(request), "PageGetter-" + pageGetterCount)
        }
      }
    case i@ImageRequest(_, rawUrl, previousRequest, depth) =>
      onRequest(i) { request =>
        imageGetterCount += 1
        context.actorOf(HttpUrlGetter.propsOfPages(i), "ImageGetter-" + imageGetterCount)
      }
    case ReceiveTimeout =>
      context.stop(self)
    case SlowDown =>
      log.info("HttpRequestDispatcher is going to slow down 30 seconds")
//      context.setReceiveTimeout(Duration.Undefined)
//      context.setReceiveTimeout(30 seconds)
//      context.become(waiting)
  }

  def waiting: Receive = {
    case ReceiveTimeout =>
      if (queue.size > 0) {
        context.unbecome
        self ! queue.head
        queue = queue.drop(1)
      }
    case message: HttpRequest =>
      queue = queue ::: message :: Nil
    case _ =>
      log.info("HttpRequestDispatcher is going to slow down 30 seconds")
      context.setReceiveTimeout(Duration.Undefined)
      context.setReceiveTimeout(30 seconds)
  }

  private def needVisit(url: String, depth: Int): Boolean = {
    !visitedUrls.contains(url) && !exclude(url) && !url.contains("#") && url.length > 10 //depth <= maxDepth &&
  }

  private def obeyDomainConstraints(url: String): Boolean = {
    val domainPrefixReg = new Regex( """(http|https)://.*?/""")
    val domainUrl: String = domainPrefixReg.findFirstIn(url).getOrElse(url)
    if (domainUrl != "") {
      domainConstraints.exists(x => domainUrl.contains(x))
    }
    else false
  }

}

object SlowDown

object HttpRequestDispatcher {
  val name: String = "HttpRequestDispatcher"
  val path = "akka://Main/user/app/" + name

  def props(headers: Map[String, String], excludes: Set[String], initialUrl: String, domainConstraints: Set[String], maxDepth: Int) = Props(classOf[HttpRequestDispatcher], headers, excludes, initialUrl, domainConstraints, maxDepth)


}