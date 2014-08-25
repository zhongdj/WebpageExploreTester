package net.imadz.web.explorer

import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive

/**
 * Created by Scala on 14-8-25.
 */
class UrlRepository extends Actor{
  import UrlRepository._

  override def receive: Receive = {
    case Push(request) => {
      //
    }
    case Pop(n) => {

    }
  }
}

object UrlRepository {
  var cache = List[HttpRequest]

  case class Push(request: HttpRequest)
  case class Pop(n: Int)

  def props = Props(classOf[UrlRepository], "urlRepository")
}
