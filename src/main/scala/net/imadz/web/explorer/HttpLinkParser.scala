package net.imadz.web.explorer

import akka.actor.{ActorLogging, Props, ActorRef, Actor}
import akka.actor.Actor.Receive
import akka.event.LoggingReceive
import net.imadz.web.explorer._

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * Created by geek on 8/20/14.
 */
class HttpLinkParser(body: String, httpRequest: PageRequest, dispatcher: ActorRef) extends Actor with ActorLogging {

  self ! body

  override def receive: Receive = LoggingReceive {
    case body: String =>
      parse(body) { request =>
        dispatcher ! request
      }
  }

  private def parse(body: String)(dispatch: HttpRequest => Unit) = {
    findLinks(body) foreach (newLink => dispatch(newLink))
  }

  val A_TAG = """(?s)(?i)<a (.*)>(.+)?</a>""".r
  val HREF_ATTR = """(?s)\s+(?i)href\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))\s*""".r

  def findPageLinks(body: String): Iterator[String] = {
    for {
      A_TAG(attrs, name) <- A_TAG findAllIn body
      HREF_ATTR(dquot, quot, bare) <- HREF_ATTR findAllIn attrs
    } yield if (dquot != null) dquot trim
    else if (quot != null) quot trim
    else bare trim
  }


  val IMG_TAG = "(?s)(?i)<img ([^>]+)>.+?</img>".r
  val IMG_SRC = """\s*(?i)src\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))\s*""".r

  def findImageLinks(body: String): Iterator[String] = {
    for {
      anchor <- IMG_TAG.findAllMatchIn(body)
      IMG_SRC(dquot, quot, bare) <- anchor.subgroups
    } yield if (dquot != null) dquot trim
    else if (quot != null) quot trim
    else bare trim
  }


  def processUrl(rawUrl: String, contextUrl: String): String = {
    val domainRegex = new Regex( """(http|https)://.*?/""")
    val domainUrl = domainRegex.findFirstIn(contextUrl).getOrElse(contextUrl + "/")
    if (rawUrl.startsWith("http") || rawUrl.startsWith("https")) rawUrl
    else if (rawUrl.startsWith("/")) {
      if (domainUrl.endsWith("/"))
        domainUrl.take(domainUrl.length - 1) + rawUrl
      else
        domainUrl + rawUrl
    }
    else {
      val prefixUrl = new Regex( """(http|https)://.*/""").findFirstIn(contextUrl).getOrElse(contextUrl)
      prefixUrl + rawUrl
    }
  }

  def trancateInavlidsChars(rawUrl: String): String = {
    val invalidChars = '|' :: ' ' :: '\\' :: Nil
    val invalidIndexes = invalidChars map {rawUrl.indexOf(_)} filter {_ > 0}
    val minIndex = invalidIndexes.foldLeft(Int.MaxValue){(min, index) => Math.min(min, index)}
    if (minIndex < Int.MaxValue) rawUrl.substring(0, minIndex)
    else rawUrl
  }

  private def findLinks(body: String): List[HttpRequest] = {
    val pages: List[PageRequest] = findPageLinks(body) filter (invalids)  map { rawUrl => processUrl(rawUrl, httpRequest.url)
    } map {rawUrl => trancateInavlidsChars(rawUrl)} map { url => PageRequest(httpRequest.headers, url, Some(httpRequest), httpRequest.depth + 1)
    } toList

    val images: List[ImageRequest] = findImageLinks(body) map { url =>
      ImageRequest(httpRequest.headers, url, httpRequest.asInstanceOf[PageRequest], httpRequest.depth + 1)
    } toList

    pages ::: images
  }

  def invalids: (String) => Boolean = {
    rawUrl =>
      rawUrl.size > 0 && !rawUrl.contains("#") && !rawUrl.contains("javascript") && !rawUrl.startsWith("mailto:")
  }
}



object HttpLinkParser {

  def props(body: String, httpRequest: PageRequest, dispatcher: ActorRef) = Props(classOf[HttpLinkParser], body, httpRequest, dispatcher)

}

