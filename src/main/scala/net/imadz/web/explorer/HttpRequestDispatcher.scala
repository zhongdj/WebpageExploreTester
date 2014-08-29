package net.imadz.web.explorer

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.UrlBank.{Deposit, Payback, WithDraw}

import scala.concurrent.duration._
import scala.util.matching.Regex

/**
 * Created by geek on 8/20/14.
 */
class HttpRequestDispatcher(val urlBank: ActorRef) extends Actor with ActorLogging {

  private var pageGetterCount: Int = 0
  private var imageGetterCount: Int = 0
  private val getterMaxCount = context.system.settings.config.getInt("imadz.web.explorer.getterCount")

  urlBank ! WithDraw(getterMaxCount)

  override def receive: Receive = processRequest

  private def resetTimeout(block: => Unit) = {
    context.setReceiveTimeout(Duration.Undefined)
    block
    context.setReceiveTimeout(90 seconds)
  }

  private def processRequest: Receive = LoggingReceive {
    case Payback(requests) =>
      resetTimeout {
        requests foreach {
          case request: PageRequest =>
            pageGetterCount += 1
            context.watch(context.actorOf(HttpUrlGetter.propsOfPages(request), "PageGetter-" + pageGetterCount))
          case request: ImageRequest =>
            imageGetterCount += 1
            context.watch(context.actorOf(HttpUrlGetter.propsOfPages(request), "ImageGetter-" + imageGetterCount))
        }
      }
    case ReceiveTimeout =>
      context.stop(self)
    case Terminated(child) =>
      resetTimeout {
        urlBank ! WithDraw(1)
      }
  }
}

object HttpRequestDispatcher {
  val name: String = "HttpRequestDispatcher"
  val path = Main.path + name

  def props(urlBank: ActorRef) = Props(classOf[HttpRequestDispatcher], urlBank)
}