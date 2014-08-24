package net.imadz.web.explorer

import akka.actor.{Actor, ActorLogging, Props, Terminated}
import akka.event.LoggingReceive

/**
 * Created by geek on 8/20/14.
 */
class Main extends Actor with ActorLogging {

  //val initialUrl = "http://hybris51-prod.benefitdigital.com.cn/";

  val initialUrl = "http://m.nike.com/cn/zh_cn/";

  def headers =
    Map[String, String](
      "Accept" -> "image/webp,*/*;q=0.8",
//      "Accept-Encoding" -> "gzip,deflate,sdch",
//      "Accept-Language" -> "en-US,en;q=0.8,zh-CN;q=0.6",
//      "Cache-Control" -> "no-cache",
//      "Connection" -> "keep-alive",
//      "Pragma" -> "no-cache",
//      "Referer" -> "http://www.se566.com/html/se/79306.html",
      "User-Agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.94 Safari/537.36"
    )


  val excludes = Set[String](
    "http://www.ping.com",
    "http://help-zh-cn.swoosh.com",
    "http://nikeplus.nike.com"
  )

  val domainConstraints = Set("hybris51-prod.benefitdigital.com.cn", "nike.com")

  val dispatcher = context.actorOf(HttpRequestDispatcher.props(headers, excludes, initialUrl, domainConstraints, 100), HttpRequestDispatcher.name)
  context.watch(dispatcher)

  val httpErrorRecorder = context.actorOf(HttpErrorRecorder.props(), HttpErrorRecorder.name)

  val parserLead = context.actorOf(Props(classOf[ParserLead], dispatcher), ParserLead.name)

  //val imageDownloadLead = context.actorOf(Props(classOf[ImgDownloadLead]), ImgDownloadLead.name)

  override def receive: Receive = LoggingReceive {
    case url: String => context.actorOf(HttpRequestDispatcher.props(headers, excludes, url, domainConstraints, 10))
    case Terminated(dispatcherActor) =>
      context.children foreach context.stop
      context.stop(self)
      context.system.shutdown
      AsyncWebClient.shutdown
  }


}
