package net.imadz.web.explorer

import akka.actor._
import net.imadz.web.explorer.StateUpdate.{Processed, ShuttingDown, Stopped, UrlQueueSize}
import net.imadz.web.explorer.UrlBank._

import scala.concurrent.duration._
import scala.util.matching.Regex

/**
 * Created by Scala on 14-8-25.
 */
class UrlBank(val excludes: Set[String], val domainConstraints: Set[String], val maxDepth: Int, val observer: Option[ActorRef]) extends Actor with FSM[State, Data] with ActorLogging {

  startWith(Empty, NoHttpRequest)
  var visitedUrls = Set[String]()

  when(Empty) {
    onEvent {
      case Event(WithDraw(n), NoHttpRequest) => goto(InDebt) using Debt(n, sender)
      case Event(Deposit(requests), NoHttpRequest) => goto(Abundance) using deposit(requests)
      case Event(Shutdown, NoHttpRequest) =>
        notifyShuttingdown
        goto(WaitingForShutdown)
    }
  }

  when(InDebt) {
    onEvent {
      case Event(WithDraw(m), Debt(n, dispatcher)) =>
        stay using Debt(n + m, dispatcher)
      case Event(Deposit(requests), Debt(n, dispatcher)) =>
        if (requests.size > n) goto(Abundance) using {
          dispatcher ! Payback(requests.take(n))
          deposit(requests.drop(n))
        } else if (requests.size == n) goto(Empty) using {
          dispatcher ! Payback(requests)
          NoHttpRequest
        } else stay using {
          dispatcher ! Payback(requests)
          Debt(n - requests.size, dispatcher)
        }
      case Event(Shutdown, _) =>
        notifyShuttingdown
        goto(WaitingForShutdown)

    }
  }

  when(Abundance) {
    onEvent {
      case Event(WithDraw(n), asset: Asset) =>
        processWithDrawOnAbundance(n, asset)
      case Event(Deposit(requests), asset: Asset) =>
        processWithDepositOnAbundance(requests, asset)
      case Event(Shutdown, asset@Asset(_, cachedList)) =>
        notifyShuttingdown
        persist(cachedList)
        goto(WaitingForShutdown)
    }
  }

  when(WaitingForShutdown, 10 seconds) {
    onEvent {
      case Event(StateTimeout, _) =>
        for (listener <- observer) listener ! Stopped(self)
        context stop self
        stay
      case Event(Deposit(requests), _) => {
        persist(requests)
        stay
      }
      case otherEvent => stay
    }
  }

  private def onEvent(processEvents: => StateFunction): PartialFunction[Event, State] = {
    filterInvalidUrls andThen logEventStart andThen (stayWhileNoRequest orElse processEvents) andThen logEventEnd andThen notifyObserver
  }

  private def validateFunc: HttpRequest => Boolean = {
    case request: HttpRequest => needVisit(request, request.depth)
  }

  private def filterInvalidUrls: scala.PartialFunction[Event, Event] = {
    case Event(Deposit(requests), data) =>
      Event(Deposit(requests filter validateFunc), data)
    case otherEvent => otherEvent
  }

  private def stayWhileNoRequest: StateFunction = {
    case Event(Deposit(Nil), data) => stay
  }

  private def processWithDepositOnAbundance(requests: List[HttpRequest], asset: Asset) = asset match {

    case Asset(persistedTotal, cachedRequests) if cachedRequests.size == maxCacheSize =>
      stay using {
        persist(requests)
        asset.copy(persisted = persistedTotal + requests.length)
      }
    case Asset(_, cachedRequests) if requests.length + cachedRequests.size <= maxCacheSize =>
      stay using {
        asset.copy(cache = cachedRequests ::: requests)
      }
    case Asset(persistedTotal, cachedRequests) if requests.size + cachedRequests.length > maxCacheSize =>
      stay using {
        val leftNumber = maxCacheSize - cachedRequests.size
        persist(requests.drop(leftNumber))
        Asset(persistedTotal + leftNumber, cachedRequests ::: requests.take(leftNumber))
      }
  }

  private def payBackRequests(receive: ActorRef, requests: List[HttpRequest]) = {
    requests.foreach(r => visitedUrls += r.url)
    receive ! Payback(requests)
  }

  private def processWithDrawOnAbundance(n: Int, asset: Asset) = asset match {
    case Asset(_, cachedRequests) if cachedRequests.size > n =>
      stay using {
        payBackRequests(sender, cachedRequests.take(n))
        asset.copy(cache = cachedRequests.drop(n))
      }
    case Asset(0, cachedRequests) if cachedRequests.size == n =>
      goto(Empty) using {
        payBackRequests(sender, cachedRequests)
        NoHttpRequest
      }
    case Asset(0, cachedRequests) if cachedRequests.size < n =>
      goto(InDebt) using {
        payBackRequests(sender, cachedRequests)
        Debt(n - cachedRequests.size, sender)
      }
    case Asset(persistedTotal, cachedRequests) if cachedRequests.size == n =>
      stay using {
        payBackRequests(sender, cachedRequests)
        fillCache(persistedTotal)
      }
    case Asset(persistedTotal, cachedRequests) if cachedRequests.size < n =>
      if (persistedTotal + cachedRequests.size == n) goto(Empty) using {
        payBackRequests(sender, cachedRequests ::: pop(persistedTotal))
        NoHttpRequest
      }
      else if (persistedTotal + cachedRequests.size > n) stay using {
        payBackRequests(sender, cachedRequests ::: pop(n - cachedRequests.size))
        fillCache(persistedTotal + cachedRequests.size - n)
      } else goto(InDebt) using {
        payBackRequests(sender, cachedRequests ::: pop(persistedTotal))
        Debt(n - persistedTotal - cachedRequests.size, sender)
      }
  }


  private def dataStr(data: Data): String = data match {
    case NoHttpRequest => "NoHttpRequest"
    case d: Debt => d toString
    case Asset(persisted, cache) => "Asset(persisted = " + persisted + ", cacheSize =" + cache.size + ")"
  }


  private def logEventStart: scala.PartialFunction[Event, Event] = {
    case event: Event =>
      log.debug("----------------------------------------------------------------------------")
      log.debug("Event: " + event.event + ", oldState: " + this.stateName + ", oldStateData: " + dataStr(event.stateData))
      event
  }

  private def notifyShuttingdown: Unit = for (listener <- observer) listener ! ShuttingDown(self)

  private def notifyProcessed: Unit = for (listener <- observer) yield listener ! Processed(visitedUrls.size)

  private def notifyUrlQueueSize(size: Int): Unit = for (listener <- observer) yield listener ! UrlQueueSize(size)

  private def notifyObserver: PartialFunction[State, State] = {
    case state: State => state.stateName match {
      case Empty =>
        notifyProcessed
        notifyUrlQueueSize(0)
      case InDebt =>
        notifyProcessed
        notifyUrlQueueSize(0)
      case Abundance =>
        val asset = state.stateData.asInstanceOf[Asset]
        notifyProcessed
        notifyUrlQueueSize(asset.cache.size + asset.persisted)
      case WaitingForShutdown =>
        notifyProcessed
    }
      state
  }

  private def logEventEnd: scala.PartialFunction[State, State] = {
    case state: State =>
      log.debug("newState: " + state.stateName + ", stateData: " + dataStr(state.stateData))
      log.debug("----------------------------------------------------------------------------")
      state
  }


  def fillCache(persistedLeft: Int): Asset = {
    if (persistedLeft >= maxCacheSize) Asset(persistedLeft - maxCacheSize, pop(maxCacheSize))
    else Asset(0, pop(persistedLeft))
  }

  def deposit(requests: List[HttpRequest]): Asset = {
    if (requests.size <= maxCacheSize) Asset(0, requests)
    else {
      persist(requests.drop(maxCacheSize))
      Asset(requests.size - maxCacheSize, requests.take(maxCacheSize))
    }
  }

  def needVisit(request: HttpRequest, depth: Int): Boolean = {
    if (request.previousRequest.isDefined) {
      !visitedUrls.contains(request.url) &&
        !exclude(request.url) &&
        !request.url.contains("#") &&
        request.url.length > 10 &&
        depth <= maxDepth &&
        obeyDomainConstraints(request.previousRequest.get.url)
    } else {
      !visitedUrls.contains(request.url) &&
        !exclude(request.url) &&
        !request.url.contains("#") &&
        request.url.length > 10 &&
        depth <= maxDepth &&
        obeyDomainConstraints(request.url)
    }
  }

  private def exclude(url: String) = {
    excludes.exists(x => if (x.isEmpty) false else url.startsWith(x))
  }

  private def obeyDomainConstraints(url: String): Boolean = {
    val domainPrefixReg = new Regex( """(http|https)://.*?/""")
    val domainUrl: String = domainPrefixReg.findFirstIn(url).getOrElse(url)
    if (domainUrl != "") {
      domainConstraints.exists(keyword => domainUrl.contains(keyword))
    }
    else false
  }

  var db = List[HttpRequest]()

  private def persist(requests: List[HttpRequest]) = {
    db = db ::: requests
  }

  private def pop(quantity: Int): List[HttpRequest] = {
    val old = db
    db = db.drop(quantity)
    old.take(quantity)
  }

  initialize()

}

object UrlBank {
  val name: String = "UrlBank"

  def apply(context: ActorContext, run: TestRun, observer: Option[ActorRef]): ActorRef =
    context.actorOf(props(run.exclusionList, run.inclusionList, run.depth, observer), UrlBank.name)

  def props(ex: Set[String], in: Set[String], depth: Int, observer: Option[ActorRef]): Props = Props(classOf[UrlBank], ex, in, depth, observer)

  val maxCacheSize = 2000

  var cache = List[HttpRequest]()

  //received events
  final case class Deposit(requests: List[HttpRequest])

  final case class WithDraw(n: Int)

  //send events
  final case class Payback(requests: List[HttpRequest])

  //state
  // states
  sealed trait State

  case object Empty extends State

  case object InDebt extends State

  case object Abundance extends State

  case object WaitingForShutdown extends State

  sealed trait Data

  case object NoHttpRequest extends Data

  case class Debt(n: Int, dispatcher: ActorRef) extends Data

  case class Asset(persisted: Int, cache: List[HttpRequest]) extends Data

  def props(excludes: Set[String], domainConstraints: Set[String], maxDepth: Int) = Props(classOf[UrlBank], excludes, domainConstraints, maxDepth, "urlRepository")

}