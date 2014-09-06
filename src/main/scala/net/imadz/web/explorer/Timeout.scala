package net.imadz.web.explorer

import akka.actor.ActorContext
import scala.concurrent.duration._

/**
 * Created by Scala on 14-9-6.
 */
trait Timeout {
  def resetTimeout(context: ActorContext)(block: => Unit)(implicit timeout: Duration = 90 seconds) = {
    context.setReceiveTimeout(Duration.Undefined)
    block
    context.setReceiveTimeout(timeout)
  }
}
