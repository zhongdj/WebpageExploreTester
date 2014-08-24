package net.imadz.web.explorer

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive

import scala.util.matching.Regex

/**
 * Created by geek on 8/20/14.
 */
class HttpLinkParser(body: String, httpRequest: PageRequest, dispatcher: ActorRef) extends Actor with ActorLogging {

  //TODO url encode

  self ! body

  override def receive: Receive = LoggingReceive {
    case body: String =>
      parse(body) { request =>
        dispatcher ! request
      }
      context.stop(self)
  }

  private def parse(body: String)(dispatch: HttpRequest => Unit) = {
    try {
       findLinks(body) foreach (newLink => dispatch(newLink))
    } catch {
      case t =>
        context.stop(self)
        throw t
    }
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


  val IMG_TAG = "(?s)(?i)<img ([^<]+)>".r
  val IMG_SRC = """\s*(?i)src\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))\s*""".r

  def findImageLinks(body: String): Iterator[String] = {
    for {
      IMG_TAG(anchor) <- IMG_TAG.findAllIn(body)
      IMG_SRC(dquot, quot, bare) <- IMG_SRC findAllIn (anchor)
    } yield if (dquot != null) dquot trim
    else if (quot != null) quot trim
    else bare trim
  }


  def processUrl(rawUrl: String, contextUrl: String): String = {
    val domainRegex = new Regex( """(http|https)://.*?/""")
    val domainUrl = domainRegex.findFirstIn(contextUrl).getOrElse(contextUrl + "/")
    if (rawUrl.startsWith("http") || rawUrl.startsWith("https")) rawUrl
    else if (rawUrl.startsWith("//")) {
       "http:" + rawUrl
    } else if (rawUrl.startsWith("/")) {
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
    val invalidChars = '|'  :: '\\' :: '\"' :: '\'' :: '{' :: '[' :: Nil
    val invalidIndexes = invalidChars map {
      rawUrl.indexOf(_)
    } filter {
      _ > 0
    }
    val minIndex = invalidIndexes.foldLeft(Int.MaxValue) { (min, index) => Math.min(min, index)}
    if (minIndex < Int.MaxValue) rawUrl.substring(0, minIndex)
    else rawUrl
  }

  private def findLinks(body: String): List[HttpRequest] = {
    val pages: List[PageRequest] = findPageLinks(body) filter (invalids) map { rawUrl => processUrl(rawUrl, httpRequest.url)
    } map { rawUrl => trancateInavlidsChars(rawUrl)} map { url => PageRequest(httpRequest.headers, url, Some(httpRequest), httpRequest.depth + 1)
    } toList

    /*
    val images: List[ImageRequest] = findImageLinks(body) filter (invalids) map { rawUrl =>
      val newImageUrl = processUrl(rawUrl, httpRequest.url)
      //      log.error("---------------------------------------------------------------------------------")
      //      log.error("orignial image url: " + rawUrl)
      //      log.error("processed imageUrl: " + newImageUrl)
      newImageUrl
    } map { rawUrl => trancateInavlidsChars(rawUrl)} map { url =>
      ImageRequest(httpRequest.headers, url, httpRequest.asInstanceOf[PageRequest], httpRequest.depth + 1)
    } toList

    pages ::: images
    */
    pages
  }

  def invalids: (String) => Boolean = {
    rawUrl =>
      rawUrl.size > 0 && !rawUrl.contains("#") && !rawUrl.contains("javascript") && !rawUrl.startsWith("mailto:")
  }
}


object HttpLinkParser {

  def props(body: String, httpRequest: PageRequest, dispatcher: ActorRef) = Props(classOf[HttpLinkParser], body, httpRequest, dispatcher).withDispatcher("parser-dispatcher")

}

