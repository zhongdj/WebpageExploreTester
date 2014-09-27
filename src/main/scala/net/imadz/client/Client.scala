package net.imadz.client

import akka.actor._
import net.imadz.web.explorer.HttpError
import net.imadz.web.explorer.StateUpdate._

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
  //  val system: ActorSystem = ActorSystem("spiderman")
  //  val cl = system.actorOf(Props(classOf[Client]), "Client")
  //  val engine = system.actorOf(Main.props(Some(cl), Some(cl)), Main.name)
  //
  //  val targetUrl: String = "http://www.nike.com/cn/zh_cn/"
  //  val deviceType: String = "Desktop"
  //  val countryAbbr: String = "CN"
  //
  //  val headerText: String = ""
  //
  //  val exclusions: String = ""
  //
  //  val inclusions: String =
  //    """
  //      |m.nike.com/cn/zh_cn/
  //      |store.nike.com/cn/zh_cn/
  //      |secure-store.nike.com/cn/zh_cn/
  //    """.stripMargin
  //
  //  val checkImage: Boolean = true
  //
  //  val depth: Int = 10
  //
  //  engine ! TestRun(targetUrl, deviceType, countryAbbr, headerText, exclusions, inclusions, checkImage, depth)

  val text = """<div class="nike-cq-subtitle"><div class="nike-cq-subtitle-component nike-cq-touts-full-screen-tout-touts-labels-2-resource"><h2 class="nike-cq-subtitle-headline nike-cq-subtitle-headline-level-2 nike-cq-subtitle-styled-header"><span class="nike-cq-subtitle-line-1 nike-cq-title-line nike-cq-line1 nike-cq-color-e24c1d18-0b26-4724-85b7-d9d7b9e5a28f nike-cq-font32px nike-cq-spacing04px nsg-font-family--platform">NIKE FLYKNIT</span> <span class="nike-cq-subtitle-line-2 nike-cq-title-line nike-cq-line2 nike-cq-color-e24c1d18-0b26-4724-85b7-d9d7b9e5a28f nike-cq-font16px nike-cq-nospacing nsg-font-family--base">轻盈出奇，强韧出众。</span></h2></div></div>, go to page http://www.nike.com/cn/zh_cn/c/innovation/flyknit""".stripMargin

  val COMP_TAG2 = """(?s)(?i)<span\s?[^<]*>([^>]*)</span>""".r

  val pattern = """<[^>]+>([^<]+)<""".r
  val matches = pattern.findAllMatchIn(text)

  {
    for {
      m <- matches
    } yield m.group(m.groupCount)
  }.mkString(" ").foreach(print)



}