package net.imadz.web.explorer

import akka.actor._
import net.imadz.web.explorer.UrlBank._

/**
 * Created by Scala on 14-8-25.
 */
class UrlBank extends Actor with FSM[State, Data] with ActorLogging {

  startWith(Empty, NoHttpRequest)

  def dataStr(data: Data): String = data match {
    case NoHttpRequest => "NoHttpRequest"
    case d: Debt => d toString
    case Asset(persisted, cache) => "Asset(persisted = " + persisted + ", cacheSize =" + cache.size + ")"
  }

  def wrap(sf: StateFunction): StateFunction = {

    case event: Event =>
      log.info("----------------------------------------------------------------------------")
      log.info("Event: " + event.event + ", oldState: " + this.stateName + ", oldStateData: " + dataStr(event.stateData))
      val state = sf(event)
      log.info("newState: " + state.stateName + ", stateData: " + dataStr(state.stateData))
      log.info("----------------------------------------------------------------------------")

      state
  }

  when(Empty) {
    wrap {
      case Event(WithDraw(n), NoHttpRequest) =>
        goto(InDebt) using Debt(n, sender)
      case Event(Deposit(requests), NoHttpRequest) =>
        goto(Abundance) using deposit(requests)

    }
  }
  when(InDebt) {
    wrap {
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
    }
  }


  when(Abundance) {
    wrap {
      case Event(WithDraw(n), asset: Asset) =>
        processWithDrawOnAbundance(n, asset)
      case Event(Deposit(requests), asset: Asset) =>
        processWithDepositOnAbudance(requests, asset)
    }
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

  private def processWithDrawOnAbundance(n: Int, asset: Asset) = asset match {
    case Asset(_, cachedRequests) if cachedRequests.size > n =>
      stay using {
        sender ! Payback(cachedRequests.take(n))
        asset.copy(cache = cachedRequests.drop(n))
      }
    case Asset(0, cachedRequests) if cachedRequests.size == n =>
      goto(Empty) using {
        sender ! Payback(cachedRequests)
        NoHttpRequest
      }
    case Asset(0, cachedRequests) if cachedRequests.size < n =>
      goto(InDebt) using {
        sender ! Payback(cachedRequests)
        Debt(n - cachedRequests.size, sender)
      }
    case Asset(persistedTotal, cachedRequests) if cachedRequests.size == n =>
      stay using {
        sender ! Payback(cachedRequests)
        fillCache(persistedTotal)
      }
    case Asset(persistedTotal, cachedRequests) if cachedRequests.size < n =>
      if (persistedTotal + cachedRequests.size == n) goto(Empty) using {
        sender ! Payback(cachedRequests ::: pop(persistedTotal))
        NoHttpRequest
      }
      else if (persistedTotal + cachedRequests.size > n) stay using {
        sender ! Payback(cachedRequests ::: pop(n - cachedRequests.size))
        fillCache(persistedTotal + cachedRequests.size - n)
      } else goto(InDebt) using {
        sender ! Payback(cachedRequests ::: pop(persistedTotal))
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

  final case class WithDraw(n: Int)

  //send events
  final case class Payback(requests: List[HttpRequest])

  //state
  // states
  sealed trait State

  case object Empty extends State

  case object InDebt extends State

  case object Abundance extends State

  sealed trait Data

  case object NoHttpRequest extends Data

  case class Debt(n: Int, dispatcher: ActorRef) extends Data

  case class Asset(persisted: Int, cache: List[HttpRequest]) extends Data

  def props = Props(classOf[UrlBank], "urlRepository")
}
