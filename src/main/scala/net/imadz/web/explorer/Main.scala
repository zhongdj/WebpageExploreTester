
package net.imadz.web.explorer

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.StateUpdate.Stopped
import net.imadz.web.explorer.UrlBank.Deposit

import scala.collection.immutable.ListSet
import scala.concurrent.duration._

/**
 * Created by geek on 8/20/14.
 */
class Main(errorHandler: Option[ActorRef], observer: Option[ActorRef]) extends Actor with ActorLogging {

  override def receive: Receive = LoggingReceive {
    case run: TestRun =>

      val urlBank = UrlBank(context, run, observer)
      urlBank ! Deposit(ListSet(PageRequest(run.headers, run.targetUrl, "Landing Page", None, 0)))

      val dispatcher = HttpRequestDispatcher(context, urlBank, observer)
      val errorRecorder = HttpErrorRecorder(context, errorHandler, observer)
      val parserLead = ParserLead(context, urlBank, observer)
      val imageLead = context.actorOf(Props(classOf[ImgDownloadLead], urlBank), ImgDownloadLead.name)

      context.children.foreach {
        context watch
      }
      context.become(running(urlBank, dispatcher, errorRecorder, parserLead, imageLead))
    case Shutdown => shutdown
  }

  def shutdown {
    AsyncWebClient.shutdown
    for (listener <- observer) yield listener ! Stopped(self)
    context stop self
    context.system.shutdown
  }

  def running(urlBank: ActorRef, dispatcher: ActorRef, errorRecorder: ActorRef, parserLead: ActorRef, imageLead: ActorRef): Receive = {
    case Terminated(child) =>
      if (context.children.isEmpty) {
        shutdown
      }
      else if (child.equals(dispatcher)) self ! Shutdown
    case Shutdown =>
      context.children foreach {
        _ ! Shutdown
      }
      context.setReceiveTimeout(15 seconds)
    case ReceiveTimeout => shutdown
  }
}

//Request for shutdown
object Shutdown

//status update for observer
object StateUpdate {

  case class UrlQueueSize(number: Int)

  case class Getting(number: Int)

  case class Parsing(number: Int)

  case class Processed(number: Int)

  case class Stopped(actor: ActorRef)

  case class ShuttingDown(actor: ActorRef)

  case class HttpErrorFound(error: HttpError)

}

case class TestRun(targetUrl: String, headerText: String, exclusions: String, inclusions: String, checkImage: Boolean, depth: Int) {
  val seperator: String = "\n"

  def exclusionList = exclusions.split(seperator).map(_.trim).toSet

  def inclusionList = inclusions.split(seperator).map(_.trim).toSet

  def headers: Map[String, String] = {
    val result: List[(String, String)] = headerText.split(seperator).toList.map { line: String =>
      val separator = line.indexOf(":")
      (line.substring(0, separator).trim, line.substring(separator + 1).trim)
    }

    result.toMap ++ Map[String, String](
      "Accept" -> "text/html,application/xhtml+xml,text/xml,application/xml;q=0.9,image/webp,image/GIF,image/jpeg,text/plain, image/png, image/tiff, image/x-icon"
    )
  }
}

object Main {
  val name = "Main"
  val path = "akka://spiderman/user/" + name + "/"

  def props(errorHandler: Option[ActorRef], observer: Option[ActorRef]) = Props(classOf[Main], errorHandler, observer)

}