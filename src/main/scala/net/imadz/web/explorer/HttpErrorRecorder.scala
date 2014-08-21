package net.imadz.web.explorer

import java.io.{File, PrintWriter}

import akka.actor.{Actor, ActorLogging, Props}
import akka.event.LoggingReceive
import net.imadz.web.explorer.HttpErrorRecorder.HttpError

/**
 * Created by geek on 8/20/14.
 */
class HttpErrorRecorder extends Actor with ActorLogging {

  var writer : PrintWriter= _

  override def receive: Receive = LoggingReceive {
    case HttpError(responseCode, pageRequest: PageRequest) =>
      logPageError(responseCode, pageRequest)
    case HttpError(responseCode, imageRequest: ImageRequest) =>
      logImageError(responseCode, imageRequest)
  }

  def logPageError(responseCode: Int, request: PageRequest): Unit = {
    println("response code: " + responseCode + ", page url: " + request.url)
    writer.println(request.toString)
    writer.flush()

  }

  def logImageError(responseCode: Int, request: ImageRequest): Unit = {
    println("response code: " + responseCode + ", image url: " + request.url)
    writer.println(request.toString)
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
