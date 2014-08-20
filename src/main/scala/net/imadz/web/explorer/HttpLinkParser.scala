package net.imadz.web.explorer

import akka.actor.{Props, ActorRef, Actor}
import akka.actor.Actor.Receive
import net.imadz.web.explorer._

import scala.collection.mutable.ListBuffer

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

//  val A_TAG = "(?s)(?i)<a ([^>]+)>.+?</a>".r
  val A_TAG = "(?m)(?i)href=.*".r
  val HREF_ATTR = """\s*(?i)href\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))\s*""".r

//  def findPageLinks(body: String): Iterator[String] = {
//    for {
//      anchor <- A_TAG.findAllMatchIn(body)
//      HREF_ATTR(dquot, quot, bare) <- anchor.subgroups
//    } yield if (dquot != null) dquot
//    else if (quot != null) quot
//    else bare
//  }
abstract class State {
  def read(line: String, buffer: ListBuffer[String]) : State
}


  object PendingAnchorState extends State {
    override def read(line: String, buffer: ListBuffer[String]): State = {
      if (line.contains("<a")) {
        if (!line.contains("href")) {
          PendingHrefState
        } else {
          val i = line.indexOf("href")
          val nohref = line.substring(i)
          val q1 = nohref.indexOf("\"")
          val noq1 = nohref.substring(q1 + 1)
          val q2 = noq1.indexOf("\"")
          val url: String = noq1.substring(0, q2)
          if (!url.startsWith("http")) {
            buffer += "http://www.nike.com" + url
          } else {
            buffer += url
          }
          PendingAnchorState
        }
      } else {
        PendingAnchorState
      }

    }
  }

  object PendingHrefState extends State {
    override def read(line: String, buffer: ListBuffer[String]): State = {
      if (!line.contains("href")) {
        PendingHrefState
      } else {
        val i = line.indexOf("href")
        val nohref = line.substring(i)
        val q1 = nohref.indexOf("\"")
        println("====================================================================================")
         println (line)
        val noq1 = nohref.substring(q1 + 1)
        val q2 = noq1.indexOf("\"")
        println("====================================================================================")
        println (noq1)

        val url: String = if (q2 < 0) noq1 else noq1.substring(0, q2)
        if (!url.startsWith("http")) {
          buffer += "http://www.nike.com" + url
        } else {
          buffer += url
        }
        PendingAnchorState
      }
    }
  }
  def findPageLinks(body: String): Iterator[String] = {
    val result = body.split("\\n").iterator.foldLeft[(ListBuffer[String], State)]((ListBuffer.empty[String], PendingAnchorState)) { case ((buffer, state), line) =>
      (buffer, state.read(line, buffer))
    }
    result._1.iterator
  }

  val IMG_TAG = "(?s)(?i)<img ([^>]+)>.+?</img>".r
  val IMG_SRC = """\s*(?i)src\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))\s*""".r

  def findImageLinks(body: String): Iterator[String] = {
    for {
      anchor <- IMG_TAG.findAllMatchIn(body)
      IMG_SRC(dquot, quot, bare) <- anchor.subgroups
    } yield if (dquot != null) dquot
    else if (quot != null) quot
    else bare
  }

  private def findLinks(body: String): List[HttpRequest] = {
    val pages: List[PageRequest] = findPageLinks(body) map { url => PageRequest(url, Some(httpRequest), httpRequest.depth + 1)} toList
    val images: List[ImageRequest] = findImageLinks(body) map { url =>
      ImageRequest(url, httpRequest.asInstanceOf[PageRequest], httpRequest.depth + 1)
    } toList

    pages ::: images
  }
}

object HttpLinkParser {

  def props(body: String, httpRequest: PageRequest, dispatcher: ActorRef) = Props(classOf[HttpLinkParser], body, httpRequest, dispatcher)

}

