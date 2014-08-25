package net.imadz.web.explorer

import akka.actor.{Actor, ActorLogging, Props, Terminated}
import akka.event.LoggingReceive

/**
 * Created by geek on 8/20/14.
 */
class Main extends Actor with ActorLogging {

  val initialUrl = "http://hybris51-prod.benefitdigital.com.cn/";

  //val initialUrl = "http://www.nike.com/cn/zh_cn/";
  val initialName = ""

  def headers =
    Map[String, String](
      "Accept" -> "image/webp,*/*;q=0.8",
//      "Accept-Encoding" -> "gzip,deflate,sdch",
//      "Accept-Language" -> "en-US,en;q=0.8,zh-CN;q=0.6",
//      "Cache-Control" -> "no-cache",
//      "Connection" -> "keep-alive",
//      "Pragma" -> "no-cache",
//      "Referer" -> "http://www.se566.com/html/se/79306.html",
      "Host" -> "hybris51-prod.benefitdigital.com.cn",
      "Authorization" -> "Basic YmVuZWZpdDpoeWJyaXM=",
      "Cookie" -> "symfony=apmod5halbtlj89pps8a3vi5t6; 38tERD42XE=%7B%22cart%22%3A%5B%5D%2C%22shoppingCartNumberOfItems%22%3A0%2C%22user%22%3Anull%2C%22shoppingCartId%22%3A%22anonymous%22%2C%22customerCountry%22%3A%22CN%22%7D",
      "User-Agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.94 Safari/537.36"
    )


  val excludes = Set[String](
    "http://www.ping.com",
    "http://help-zh-cn.swoosh.com",
    "http://nikeplus.nike.com"
  )

  val domainConstraints = Set("benefitdigital.com", "nike.com")

  val dispatcher = context.actorOf(HttpRequestDispatcher.props(headers, excludes, initialUrl, initialName,domainConstraints, 100), HttpRequestDispatcher.name)
  context.watch(dispatcher)

  val httpErrorRecorder = context.actorOf(HttpErrorRecorder.props(), HttpErrorRecorder.name)

  val parserLead = context.actorOf(Props(classOf[ParserLead], dispatcher), ParserLead.name)

  //val imageDownloadLead = context.actorOf(Props(classOf[ImgDownloadLead]), ImgDownloadLead.name)

  override def receive: Receive = LoggingReceive {
    case url: String => context.actorOf(HttpRequestDispatcher.props(headers, excludes, url, initialName, domainConstraints, 10))
    case Terminated(dispatcherActor) =>
      context.children foreach context.stop
      context.stop(self)
      context.system.shutdown
      AsyncWebClient.shutdown
  }


}

object Main {
  val name = "Main"
  val path = "akka://" + name+ "/user/app/"
}