package net.imadz.web.explorer

import akka.actor._
import akka.event.LoggingReceive
import net.imadz.web.explorer.ParserLead.{PageParsed, PageEnd}
import net.imadz.web.explorer.StateUpdate.Getting
import net.imadz.web.explorer.UrlBank.{Deposit, Payback, WithDraw}

import scala.concurrent.duration._

/**
 * Created by geek on 8/20/14.
 */
class HttpRequestDispatcher(val urlBank: ActorRef, observer: Option[ActorRef]) extends Actor with ActorLogging with Timeout {

  private var pageGetterCount: Int = 0
  private var imageGetterCount: Int = 0
  private val getterMaxCount = context.system.settings.config.getInt("imadz.web.explorer.getterCount")
  private var getterUrlMap = Map[String, String]().withDefaultValue("")

  AsyncWebClient.setGetterNumber(getterMaxCount)

  urlBank ! WithDraw(getterMaxCount)

  override def receive: Receive = processRequest

  private def processRequest: Receive = timeoutReceive orElse LoggingReceive {
    case Payback(requests) => resetTimeout(context) {
      requests foreach {
        case request: PageRequest =>
          pageGetterCount += 1
          val pageGetter: ActorRef = context.actorOf(HttpUrlGetter.propsOfPages(request, urlBank), "PageGetter-" + pageGetterCount)
          getterUrlMap += pageGetter.path.toString -> request.url
          context.watch(pageGetter)
        case request: ImageRequest =>
          imageGetterCount += 1
          context.watch(context.actorOf(HttpUrlGetter.propsOfPages(request, urlBank), "ImageGetter-" + imageGetterCount))
      }
      notifyGetting
    }
    case ReceiveTimeout =>
      shutdownNow
    case Terminated(pageGetter) => resetTimeout(context) {
      notifyGetting
      context.parent ! PageEnd(getterUrlMap(pageGetter.path.toString))
      getterUrlMap -= pageGetter.path.toString
    }
    case Shutdown => resetTimeout(context) {
      notifyShuttingdown
      context.become(timeoutReceive orElse shuttingDown)
    }(10 seconds)
    case PageParsed(url) =>
      urlBank ! WithDraw(1)
  }


  def notifyGetting {
    //for (listener <- observer) yield listener ! Getting(context.children.size)
  }

  private def timeoutReceive: Receive = {
    case ReceiveTimeout => context stop self
  }

  private def shuttingDown: Receive = {
    case Payback(requests) =>
      urlBank ! Deposit(requests)
    case Terminated(child) if context.children.isEmpty =>
      shutdownNow
  }

  def shutdownNow {
    notifyStopped
    context stop self
  }

  def notifyStopped {
    for (listener <- observer) yield listener ! StateUpdate.Stopped(self)
  }

  def notifyShuttingdown {
    for (listener <- observer) listener ! StateUpdate.ShuttingDown(self)
  }
}


object HttpRequestDispatcher {
  val name: String = "HttpRequestDispatcher"
  val path = Main.path + name

  def apply(context: ActorContext, urlBank: ActorRef, observer: Option[ActorRef]) =
    context.actorOf(props(urlBank, observer), name)

  def props(urlBank: ActorRef, observer: Option[ActorRef]) = Props(classOf[HttpRequestDispatcher], urlBank, observer)
}