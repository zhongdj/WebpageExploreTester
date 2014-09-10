package net.imadz.client

import akka.actor._
import net.imadz.web.explorer.StateUpdate._
import net.imadz.web.explorer.{TestRun, PageRequest, HttpError, Main}

/**
 * Created by geek on 9/5/14.
 */
class Client extends Actor with ActorLogging {

  override def receive: Receive = {
    case e@UrlQueueSize(number: Int) =>
      log.debug(e toString)
    case e@Getting(number: Int) =>
      log.debug(e toString)
    case e@Parsing(number: Int) =>
      log.debug(e toString)
    case e@Processed(number: Int) =>
      log.debug(e toString)
    case e@Stopped(actor: ActorRef) =>
      log.debug(e toString)
    case e@ShuttingDown(actor: ActorRef) =>
      log.debug(e toString)
    case e@HttpErrorFound(error: HttpError) =>
      log.error(e toString)
  }
}

object ClientApp extends App {
  val system: ActorSystem = ActorSystem("spiderman")
  val cl = system.actorOf(Props(classOf[Client]), "Client")
  val engine = system.actorOf(Main.props(Some(cl), Some(cl)), Main.name)

  val targetUrl: String = "http://www.nike.com/cn/zh_cn/"
  val deviceType: String = "Desktop"
  val countryAbbr: String = "CN"

  val headerText: String = ""

  val exclusions: String = ""

  val inclusions: String =
    """
      |m.nike.com/cn/zh_cn/
      |store.nike.com/cn/zh_cn/
      |secure-store.nike.com/cn/zh_cn/
    """.stripMargin

  val checkImage: Boolean = true

  val depth: Int = 10

  engine ! TestRun(targetUrl, deviceType, countryAbbr, headerText, exclusions, inclusions, checkImage, depth)

}