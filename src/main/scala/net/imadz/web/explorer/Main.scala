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

case class TestRun(targetUrl: String, deviceType: String, countryAbbr: String, headerText: String, exclusions: String, inclusions: String, checkImage: Boolean, depth: Int) {
  def exclusionList = exclusions.split("\n").map(_.trim).toSet

  def inclusionList = inclusions.split("\n").map(_.trim).toSet
  
  def headers: Map[String, String] =  Map[String, String](
    "Accept" -> "text/html,application/xhtml+xml,text/xml,application/xml;q=0.9,image/webp,image/GIF,image/jpeg,text/plain, image/png, image/tiff, image/x-icon",
    //      "Accept-Encoding" -> "gzip,deflate,sdch",
    //      "Accept-Language" -> "en-US,en;q=0.8,zh-CN;q=0.6",
    //      "Cache-Control" -> "no-cache",
    //      "Connection" -> "keep-alive",
    //      "Pragma" -> "no-cache",
    //      "Referer" -> "http://www.se566.com/html/se/79306.html",
    //      "Host" -> "hybris51-prod.benefitdigital.com.cn",
    //      "Authorization" -> "Basic YmVuZWZpdDpoeWJyaXM=",
    //      "Cookie" -> "symfony=apmod5halbtlj89pps8a3vi5t6; 38tERD42XE=%7B%22cart%22%3A%5B%5D%2C%22shoppingCartNumberOfItems%22%3A0%2C%22user%22%3Anull%2C%22shoppingCartId%22%3A%22anonymous%22%2C%22customerCountry%22%3A%22CN%22%7D",
    "User-Agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.94 Safari/537.36"
  )
}

object Main {
  val name = "Main"
  val path = "akka://spiderman/user/" + name + "/"

  def props(errorHandler: Option[ActorRef], observer: Option[ActorRef]) = Props(classOf[Main], errorHandler, observer)

}