package net.imadz.web.explorer

import akka.actor.{Props, ActorRef, Actor}
import akka.actor.Actor.Receive
import net.imadz.web.explorer._

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * Created by geek on 8/20/14.
 */
class HttpLinkParser(body: String, httpRequest: PageRequest, dispatcher: ActorRef) extends Actor {

  self ! body

  override def receive: Receive = {
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
    } yield if (dquot != null) dquot
    else if (quot != null) quot
    else bare
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
    val domainRegex = new Regex("""(http|https)://.*?/""")
    val domainUrl = domainRegex.findFirstIn(contextUrl).get
    if (rawUrl.startsWith("http") || rawUrl.startsWith("https")) rawUrl
    else if(rawUrl.startsWith("/"))
    {
      if (domainUrl.endsWith("/"))
        domainUrl.take(domainUrl.length -1 ) + rawUrl
      else
        domainUrl + rawUrl
    }
    else if(rawUrl.endsWith(".html"))
    {
      if (contextUrl.endsWith("/"))
        contextUrl + rawUrl
      else
        contextUrl + "/" + rawUrl
    }
    else rawUrl
  }

  private def findLinks(body: String): List[HttpRequest] = {
    val pages: List[PageRequest] = findPageLinks(body) map {rawUrl => processUrl(rawUrl, httpRequest.url)} map { url => PageRequest(httpRequest.headers, url, Some(httpRequest), httpRequest.depth + 1)} toList
    val images: List[ImageRequest] = findImageLinks(body) map { url =>
      ImageRequest(httpRequest.headers, url, httpRequest.asInstanceOf[PageRequest], httpRequest.depth + 1)
    } toList

    pages ::: images
  }
}

object HttpLinkParser {

  def props(body: String, httpRequest: PageRequest, dispatcher: ActorRef) = Props(classOf[HttpLinkParser], body, httpRequest, dispatcher)

}

