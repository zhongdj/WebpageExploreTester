package net.imadz.web.explorer

import akka.actor._
import net.imadz.web.explorer.StateUpdate.{Processed, ShuttingDown, Stopped, UrlQueueSize}
import net.imadz.web.explorer.UrlBank._

import scala.collection.immutable.ListSet
import scala.concurrent.duration._

/**
 * Created by Scala on 14-8-25.
 */
class UrlBank(val excludes: Set[String], val inclusions: Set[String], val maxDepth: Int, val observer: Option[ActorRef]) extends Actor with FSM[State, Data] with ActorLogging {

  startWith(Empty, NoHttpRequest)
  var visitedUrls = Set[String]()

  when(Empty) {
    implicit val currentUrls: ListSet[HttpRequest] = ListSet.empty[HttpRequest]
    onEvent {
      case Event(WithDraw(n), NoHttpRequest) => goto(InDebt) using Debt(n, sender)
      case Event(Deposit(requests), NoHttpRequest) => goto(Abundance) using deposit(requests)
      case Event(Shutdown, NoHttpRequest) =>
        notifyShuttingdown
        goto(WaitingForShutdown)
    }
  }

  when(InDebt) {
    implicit val currentUrls: ListSet[HttpRequest] = ListSet.empty[HttpRequest]
    onEvent {
      case Event(WithDraw(m), Debt(n, dispatcher)) =>
        stay using Debt(n + m, dispatcher)
      case Event(Deposit(requests), Debt(n, dispatcher)) =>
        if (requests.size > n) goto(Abundance) using {
          payBackRequests(dispatcher, requests.take(n))
          deposit(requests.drop(n))
        } else if (requests.size == n) goto(Empty) using {
          payBackRequests(dispatcher, requests)
          NoHttpRequest
        } else stay using {
          payBackRequests(dispatcher, requests)
          Debt(n - requests.size, dispatcher)
        }
      case Event(Shutdown, _) =>
        notifyShuttingdown
        goto(WaitingForShutdown)

    }
  }

  when(Abundance) {
    implicit val currentUrls: ListSet[HttpRequest] = if (this.stateData.isInstanceOf[Asset])
      this.stateData.asInstanceOf[Asset].cache
    else ListSet.empty[HttpRequest]

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
    implicit val currentUrls: ListSet[HttpRequest] = ListSet.empty[HttpRequest]
    onEvent {
      case Event(StateTimeout, _) =>
        for (listener <- observer) listener ! Stopped(self)
        stop
      case Event(Deposit(requests), _) => {
        persist(requests)
        stay
      }
      case otherEvent => stay
    }
  }

  private def onEvent(processEvents: => StateFunction)(implicit currentUrls: ListSet[HttpRequest]): PartialFunction[Event, State] = {
    filterInvalidUrls(currentUrls) andThen logEventStart andThen (stayWhileNoRequest orElse processEvents) andThen logEventEnd andThen notifyObserver
  }

  private def validateFunc: HttpRequest => Boolean = {
    case request: HttpRequest => needVisit(request, request.depth)
  }

  def needVisit(request: HttpRequest, depth: Int): Boolean = {
    if (request.previousRequest.isDefined) {
      !visitedUrls.contains(request.url) &&
        !exclude(request.url) &&
        !request.url.trim.startsWith("#") &&
        request.url.length > 10 &&
        depth <= maxDepth &&
        obeyInclusions(request.previousRequest.get.url)
    } else {
      !visitedUrls.contains(request.url) &&
        !exclude(request.url) &&
        !request.url.trim.startsWith("#") &&
        request.url.length > 10 &&
        depth <= maxDepth &&
        obeyInclusions(request.url)
    }
  }

  private def filterInvalidUrls(currentUrls: ListSet[HttpRequest]): scala.PartialFunction[Event, Event] = {
    case Event(Deposit(requests), data) =>
      Event(Deposit(requests filter validateFunc filter { request =>
        !currentUrls.contains(request) && !db.contains(request)
      }), data)
    case otherEvent => otherEvent
  }

  private def stayWhileNoRequest: StateFunction = {
    case Event(Deposit(empty), data) if empty.size == 0 => stay
  }

  private def processWithDepositOnAbundance(requests: ListSet[HttpRequest], asset: Asset) = asset match {

    case Asset(persistedTotal, cachedRequests) if cachedRequests.size == maxCacheSize =>
      stay using {
        persist(requests)
        asset.copy(persisted = persistedTotal + requests.size)
      }
    case Asset(_, cachedRequests) if requests.size + cachedRequests.size <= maxCacheSize =>
      stay using {
        asset.copy(cache = cachedRequests ++ requests)
      }
    case Asset(persistedTotal, cachedRequests) if requests.size + cachedRequests.size > maxCacheSize =>
      stay using {
        val leftNumber = maxCacheSize - cachedRequests.size
        persist(requests.drop(leftNumber))
        Asset(persistedTotal + leftNumber, cachedRequests ++ requests.take(leftNumber))
      }
  }

  private def payBackRequests(receive: ActorRef, requests: ListSet[HttpRequest]) = {
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
        payBackRequests(sender, cachedRequests ++ pop(persistedTotal))
        NoHttpRequest
      }
      else if (persistedTotal + cachedRequests.size > n) stay using {
        payBackRequests(sender, cachedRequests ++ pop(n - cachedRequests.size))
        fillCache(persistedTotal + cachedRequests.size - n)
      } else goto(InDebt) using {
        payBackRequests(sender, cachedRequests ++ pop(persistedTotal))
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

  private def notifyUrlQueueSize(size: Int): Unit = {}//for (listener <- observer) yield listener ! UrlQueueSize(size)

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
      log.debug("visited Url total: " + visitedUrls.size)
      log.debug("----------------------------------------------------------------------------")
      state
  }


  def fillCache(persistedLeft: Int): Asset = {
    if (persistedLeft >= maxCacheSize) Asset(persistedLeft - maxCacheSize, pop(maxCacheSize))
    else Asset(0, pop(persistedLeft))
  }

  def deposit(requests: ListSet[HttpRequest]): Asset = {
    if (requests.size <= maxCacheSize) Asset(0, requests)
    else {
      persist(requests.drop(maxCacheSize))
      Asset(requests.size - maxCacheSize, requests.take(maxCacheSize))
    }
  }


  private def exclude(url: String) = {
    excludes.exists(x => if (x.isEmpty) false else url.startsWith(x))
  }

  private def obeyInclusions(rawUrl: String): Boolean = {
    inclusions.exists(rawUrl.contains)
  }

  var db = ListSet[HttpRequest]()

  private def persist(requests: ListSet[HttpRequest]) = {
    db = db ++ requests
  }

  private def pop(quantity: Int): ListSet[HttpRequest] = {
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
  final case class Deposit(requests: ListSet[HttpRequest])

  final case class WithDraw(n: Int)

  //send events
  final case class Payback(requests: ListSet[HttpRequest])

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

  case class Asset(persisted: Int, cache: ListSet[HttpRequest]) extends Data

  def props(excludes: Set[String], domainConstraints: Set[String], maxDepth: Int) = Props(classOf[UrlBank], excludes, domainConstraints, maxDepth, "urlRepository")

}