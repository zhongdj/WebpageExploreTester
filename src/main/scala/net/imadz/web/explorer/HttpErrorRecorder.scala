package net.imadz.web.explorer

import java.io.{FileReader, File, PrintWriter, StringReader}
import scala.io.Source

import akka.actor.{Actor, ActorLogging, Props}
import akka.event.LoggingReceive
import net.imadz.web.explorer.HttpErrorRecorder.HttpError
import net.imadz.web.explorer.utils.HttpResponseUtil
import scala.None

/**
 * Created by geek on 8/20/14.
 */
class HttpErrorRecorder extends Actor with ActorLogging {

  //TODO 408 Error

  var writer : PrintWriter= _

  override def receive: Receive = LoggingReceive {
    case HttpError(responseCode, pageRequest: PageRequest) =>
      logPageError(responseCode, pageRequest)
    case HttpError(responseCode, imageRequest: ImageRequest) =>
      logImageError(responseCode, imageRequest)
  }

  def logPageError(responseCode: Int, request: PageRequest): Unit = {
    println("response code: " + responseCode + ", page url: " + request.url)

    val bug: String = generateBug(responseCode, request)

    writer.println(bug.toString)
    writer.flush()

  }

  def generateBug(responseCode: Int,request: HttpRequest): String = {
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

  def generateBugSteps(request: HttpRequest) :String ={
     val requests = generateAllRequest(Some(request))
     val firstUrl = requests.head.url
     val tail = requests.tail
     val tailSteps =  tail map {x => "Click " + x.name + ", go to page " + x.url} mkString "\n"
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
    val bug: String = generateBug(responseCode,request)
    writer.println(bug.toString)
    writer.flush()
  }

  override def preStart(): Unit = {
    super.preStart()
    writer = new PrintWriter(new File("test.txt"))
  }

  //@throws[T](classOf[Exception])
  override def postStop(): Unit = {
    super.postStop()
    writer.close
  }
}

object HttpErrorRecorder {

  def props() = Props(classOf[HttpErrorRecorder])

  case class HttpError(responseCode: Int, httpRequest: HttpRequest)

  val name = "HttpErrorRecorder"
  val path = "akka://Main/user/app/" + name
}
