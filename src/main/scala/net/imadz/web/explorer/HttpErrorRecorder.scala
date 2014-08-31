package net.imadz.web.explorer

import java.io.{File, PrintWriter}

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.HttpErrorRecorder.HttpErrorRequest
import net.imadz.web.explorer.StateUpdate.{HttpErrorFound, ShuttingDown, Stopped}
import net.imadz.web.explorer.utils.HttpResponseUtil

import scala.concurrent.duration._
import scala.io.Source

/**
 * Created by geek on 8/20/14.
 */
class HttpErrorRecorder(errorHandler: Option[ActorRef], observer: Option[ActorRef]) extends Actor with ActorLogging {

  //TODO 408 Error

  var writer: PrintWriter = _

  private def notifyHttpError: PartialFunction[Any, HttpErrorRequest] = {
    case error: HttpErrorRequest =>
      for (handler <- errorHandler) yield handler ! HttpError(error.responseCode, error.httpRequest)
      for (listener <- observer) yield listener ! HttpErrorFound(HttpError(error.responseCode, error.httpRequest))
      error
  }

  private def logError: Receive = notifyHttpError andThen {
    case HttpErrorRequest(responseCode, pageRequest: PageRequest) =>
      logPageError(responseCode, pageRequest)
    case HttpErrorRequest(responseCode, imageRequest: ImageRequest) =>
      logImageError(responseCode, imageRequest)
  }

  override def receive: Receive = logError orElse LoggingReceive {
    case Shutdown =>
      for (listener <- observer) yield listener ! ShuttingDown(self)
      context become shuttingDown
      context.setReceiveTimeout(10 seconds)
  }

  private def shuttingDown: Receive = logError orElse LoggingReceive {
    case ReceiveTimeout =>
      for (listener <- observer) yield listener ! Stopped(self)
      context stop self
  }

  def logPageError(responseCode: Int, request: PageRequest): Unit = {
    val bug: String = generateBug(responseCode, request)

    writer.println(bug.toString)
    writer.flush()

  }

  def generateBug(responseCode: Int, request: HttpRequest): String = {
    // Read the bug template
    //    println(new File("WebTestBugTemplate.txt").getAbsolutePath)
    val bugTemplates = Source.fromURL(getClass.getResource("/WebTestBugTemplate.txt")).getLines().mkString("\n")
    // Get the title
    val title = responseCode + " error raised."
    // Get the request path -> repro steps
    val bugsteps = generateBugSteps(request)
    // Get actual result
    val actualResult = "" + responseCode + " error raised."
    // Get error Message
    val errorMessage = HttpResponseUtil.getErrorMessage(responseCode)

    // Get additional info
    val bugUrl = request.previousRequest match {
      case Some(x) => x.url
      case None => ""
    }
    // Generate a bug
    val bug = bugTemplates.replace("{title}", title)
      .replace("{bugsteps}", bugsteps)
      .replace("{actualResult}", actualResult)
      .replace("{errorMessage}", errorMessage)
      .replace("{bugUrl}", bugUrl)
    bug
  }

  def generateBugSteps(request: HttpRequest): String = {
    val requests = generateAllRequest(Some(request))
    val firstUrl = requests.head.url
    val tail = requests.tail
    val tailSteps = tail map { x => "Click " + x.name + ", go to page " + x.url} mkString "\n"
    "Go to " + firstUrl + "\n" + tailSteps
  }

  def generateAllRequest(request: Option[HttpRequest]): List[HttpRequest] = {
    request match {
      case Some(x) => generateAllRequest(x.previousRequest) ::: List(x)
      case None => List()
    }
  }

  def logImageError(responseCode: Int, request: ImageRequest): Unit = {
    println("response code: " + responseCode + ", image url: " + request.url)
    val bug: String = generateBug(responseCode, request)
    writer.println(bug.toString)
    writer.flush()
  }

  override def preStart(): Unit = {
    super.preStart()
    writer = new PrintWriter(new File("test.txt"))
  }

  override def postStop(): Unit = {
    super.postStop()
    writer.close
  }
}

case class HttpError(responseCode: Int, findInUrl: String, targetUrl: String, steps: List[NavigateSegment], raw: HttpRequest)

object HttpError {

  def stepsFrom(request: HttpRequest): List[NavigateSegment] = request match {
    case PageRequest(_, url, _, None, _) => List[NavigateSegment](LandingPage(url))
    case PageRequest(_, url, name, Some(previousPageRequest), _) => stepsFrom(previousPageRequest) :+ Step(name, url)
    case ImageRequest(_, url, _, pageRequest, _) =>  stepsFrom(pageRequest) :+ Step("Image", url)
  }

  def apply(responseCode: Int, httpRequest: HttpRequest): HttpError = {
    new HttpError(responseCode, httpRequest.url, httpRequest.previousRequest.get.url, stepsFrom(httpRequest), httpRequest)
  }
}

sealed abstract class NavigateSegment(url: String)

case class LandingPage(url: String) extends NavigateSegment(url)

case class Step(linkName: String, targetUrl: String) extends NavigateSegment(targetUrl)


object HttpErrorRecorder {

  def apply(context: ActorContext, errorHandler: Option[ActorRef], observer: Option[ActorRef]) = context.actorOf(props(errorHandler, observer), name)

  def props(errorHandler: Option[ActorRef], observer: Option[ActorRef]) = Props(classOf[HttpErrorRecorder], errorHandler, observer)

  case class HttpErrorRequest(responseCode: Int, httpRequest: HttpRequest)

  val name = "HttpErrorRecorder"
  val path = Main.path + name
}
