package net.imadz.web.explorer

import akka.actor.{ActorLogging, Props, Actor}
import akka.actor.Actor.Receive
import net.imadz.web.explorer._

/**
 * Created by geek on 8/20/14.
 */
class HttpRequestDispatcher(val headers: Map[String, String], val excludes: Set[String], val domainUrl: String, val maxDepth: Int) extends Actor with ActorLogging {

  var visitedUrls = Set[String]()

  self ! PageRequest(headers, domainUrl, None, 0)

  def exclude(url: String) = excludes.exists(url.startsWith(_))

  override def receive: Receive = {
    case p@PageRequest(_, rawUrl, previousRequest, depth) =>
      val url = rawUrl
      if (needVisit(url, depth)) {
        log.info(url)
        visitedUrls += url
        context.actorOf(HttpUrlGetter.props(p))
        log.info("cached url number: " + visitedUrls.size)
      }
    case i@ImageRequest(_, rawUrl, previousRequest, depth) =>
      val url = rawUrl
      if (needVisit(url, depth)) {
        log.info(url)
        visitedUrls += url
        context.actorOf(HttpUrlGetter.props(i))
        log.info("cached url number: " + visitedUrls.size)
      }
  }

  private def needVisit(url: String, depth: Int): Boolean = {
    depth <= maxDepth && !visitedUrls.contains(url) && !exclude(url) && !url.endsWith("#") && url.length > 10
  }


}


object HttpRequestDispatcher {

  def props(headers: Map[String, String], excludes: Set[String], initialUrl: String, maxDepth: Int) = Props(classOf[HttpRequestDispatcher], headers, excludes, initialUrl, maxDepth)

}