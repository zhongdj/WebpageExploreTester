package net.imadz.web.explorer

import akka.actor.{ReceiveTimeout, ActorLogging, Props, Actor}
import akka.actor.Actor.Receive
import net.imadz.web.explorer._
import scala.concurrent.duration._

/**
 * Created by geek on 8/20/14.
 */
class HttpRequestDispatcher(val headers: Map[String, String], val excludes: Set[String], val domainUrl: String, val maxDepth: Int) extends Actor with ActorLogging {

  var visitedUrls = Set[String]()
  var queue: List[HttpRequest] = Nil


  self ! PageRequest(headers, domainUrl, None, 0)

  def exclude(url: String) = excludes.exists(url.startsWith(_))

  var pageGetterCount: Int = 0
  var imageGetterCount: Int = 0

  context.setReceiveTimeout(10 millisecond)

  override def receive: Receive = {
    case p@PageRequest(_, rawUrl, previousRequest, depth) =>
      val url = rawUrl
      if (needVisit(url, depth)) {
        log.info(url)
        visitedUrls += url
        pageGetterCount += 1
        context.actorOf(HttpUrlGetter.props(p), "PageGetter-" + pageGetterCount)
        log.info("cached url number: " + visitedUrls.size)
      }
      //context.become(waiting, discardOld = false)
    case i@ImageRequest(_, rawUrl, previousRequest, depth) =>
      val url = rawUrl
      if (needVisit(url, depth)) {
        log.info(url)
        visitedUrls += url
        imageGetterCount += 1
        context.actorOf(HttpUrlGetter.props(i), "ImageGetter-" + imageGetterCount)
        log.info("cached url number: " + visitedUrls.size)
      }
      //context.become(waiting, discardOld = false)
  }

  def waiting : Receive = {
    case ReceiveTimeout =>
      if (queue.size > 0) {
        context.unbecome
        self ! queue.head
        queue = queue.drop(1)
      }
    case message: HttpRequest =>
      queue = queue ::: message :: Nil
    case _ =>
  }

  private def needVisit(url: String, depth: Int): Boolean = {
    !visitedUrls.contains(url) && !exclude(url) && !url.endsWith("#") && url.length > 10 //depth <= maxDepth &&
  }


}


object HttpRequestDispatcher {

  def props(headers: Map[String, String], excludes: Set[String], initialUrl: String, maxDepth: Int) = Props(classOf[HttpRequestDispatcher], headers, excludes, initialUrl, maxDepth)

}