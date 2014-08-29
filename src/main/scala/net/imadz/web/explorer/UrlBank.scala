package net.imadz.web.explorer

import akka.actor._
import net.imadz.web.explorer.UrlBank._

import scala.concurrent.duration._
import scala.util.matching.Regex

/**
 * Created by Scala on 14-8-25.
 */
class UrlBank(val excludes: Set[String], val domainConstraints: Set[String], val maxDepth: Int) extends Actor with FSM[State, Data] with ActorLogging {

  startWith(Empty, NoHttpRequest)
  var visitedUrls = Set[String]()

  def dataStr(data: Data): String = data match {
    case NoHttpRequest => "NoHttpRequest"
    case d: Debt => d toString
    case Asset(persisted, cache) => "Asset(persisted = " + persisted + ", cacheSize =" + cache.size + ")"
  }

  def validateFunc(request: HttpRequest): Boolean = needVisit(request, request.depth)

  def filterInvalidUrls: scala.PartialFunction[Event, Event] = {
    case Event(Deposit(requests), data) =>
      Event(Deposit(requests.filter(validateFunc)), data)
    case otherEvent => otherEvent
  }

  def stayWhileNoRequest: StateFunction = {
    case Event(Deposit(Nil), data) => stay
  }

  def logEventStart: scala.PartialFunction[Event, Event] = {
    case event: Event =>
      log.info("----------------------------------------------------------------------------")
      log.info("Event: " + event.event + ", oldState: " + this.stateName + ", oldStateData: " + dataStr(event.stateData))
      event
  }

  def logEventEnd: scala.PartialFunction[State, State] = {
    case state: State =>
      log.info("newState: " + state.stateName + ", stateData: " + dataStr(state.stateData))
      log.info("----------------------------------------------------------------------------")
      state
  }


  when(Empty) {
    def processEvents: StateFunction = {
      case Event(WithDraw(n), NoHttpRequest) =>
        goto(InDebt) using Debt(n, sender)
      case Event(Deposit(requests), NoHttpRequest) =>
        goto(Abundance) using deposit(requests)
      case Event(Shutdown, NoHttpRequest) =>
        goto(WaitingForShutdown)

    }
    filterInvalidUrls andThen logEventStart andThen (stayWhileNoRequest orElse processEvents) andThen logEventEnd
  }

  when(InDebt) {
    def processEvents: StateFunction = {

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
      case Event(Shutdown, NoHttpRequest) =>
        goto(WaitingForShutdown)

    }
    filterInvalidUrls andThen logEventStart andThen (stayWhileNoRequest orElse processEvents) andThen logEventEnd
  }


  when(Abundance) {
    def processEvents: StateFunction = {
      case Event(WithDraw(n), asset: Asset) =>
        processWithDrawOnAbundance(n, asset)
      case Event(Deposit(requests), asset: Asset) =>
        processWithDepositOnAbudance(requests, asset)
      case Event(Shutdown, asset@Asset(_, cachedList)) =>
        persist(cachedList)
        goto(WaitingForShutdown)
    }
    filterInvalidUrls andThen logEventStart andThen (stayWhileNoRequest orElse processEvents) andThen logEventEnd
  }

  when(WaitingForShutdown, 10 seconds) {
    case Event(StateTimeout, _) =>
      context stop self
      stay
    case Event(Deposit(requests), _) => {
      persist(requests)
      stay
    }
    case otherEvent => stay
  }

  private def processWithDepositOnAbudance(requests: List[HttpRequest], asset: Asset) = asset match {

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

  private def exclude(url: String) = excludes.exists(url.startsWith(_))

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

  val maxCacheSize = 2000

  var cache = List[HttpRequest]()

  //received events
  final case class Deposit(requests: List[HttpRequest])

  object Deposit {
    def apply(request: HttpRequest) = new Deposit(List(request))
  }

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
