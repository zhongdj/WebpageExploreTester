package net.imadz.web.explorer

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.UrlBank.{Deposit, Payback, WithDraw}

import scala.concurrent.duration._

/**
 * Created by geek on 8/20/14.
 */
class HttpRequestDispatcher(val urlBank: ActorRef) extends Actor with ActorLogging {

  private var pageGetterCount: Int = 0
  private var imageGetterCount: Int = 0
  private val getterMaxCount = context.system.settings.config.getInt("imadz.web.explorer.getterCount")

  urlBank ! WithDraw(getterMaxCount)

  override def receive: Receive = processRequest

  private def resetTimeout(block: => Unit)(timeout: Duration = 90 seconds) = {
    context.setReceiveTimeout(Duration.Undefined)
    block
    context.setReceiveTimeout(timeout)
  }

  private def processRequest: Receive = timeoutReceive orElse LoggingReceive {
    case Payback(requests) => resetTimeout {
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
    case Terminated(child) => resetTimeout {
      urlBank ! WithDraw(1)
    }
    case Shutdown => resetTimeout {
      context.become(timeoutReceive orElse shuttingDown)
    }(10 seconds)
  }

  private def shuttingDown: Receive = {
    case Payback(requests) =>
      urlBank ! Deposit(requests)
    case Terminated(child) if context.children.isEmpty =>
      context stop self
  }

  private def timeoutReceive: Receive = {
    case ReceiveTimeout => context stop self
  }
}


object HttpRequestDispatcher {
  val name: String = "HttpRequestDispatcher"
  val path = Main.path + name

  def props(urlBank: ActorRef) = Props(classOf[HttpRequestDispatcher], urlBank)
}