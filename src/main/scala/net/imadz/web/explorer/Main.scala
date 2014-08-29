package net.imadz.web.explorer

import akka.actor.{Actor, ActorLogging, Props, Terminated}
import akka.event.LoggingReceive
import scala.util.matching.Regex
import net.imadz.web.explorer.UrlBank.Deposit

/**
 * Created by geek on 8/20/14.
 */
class Main extends Actor with ActorLogging {

  //val initialUrl = "http://hybris51-prod.benefitdigital.com.cn/";

  val initialUrl = "http://help-en-cn.nike.com"
  val initialName = ""

  def headers =
    Map[String, String](
      "Accept" -> "text/html,application/xhtml+xml,text/xml,application/xml;q=0.9,image/webp,image/GIF,image/jpeg,text/plain, image/png, image/tiff, image/x-icon",
      //      "Accept-Encoding" -> "gzip,deflate,sdch",
      //      "Accept-Language" -> "en-US,en;q=0.8,zh-CN;q=0.6",
      //      "Cache-Control" -> "no-cache",
      //      "Connection" -> "keep-alive",
      //      "Pragma" -> "no-cache",
      //      "Referer" -> "http://www.se566.com/html/se/79306.html",
      //      "Host" -> "hybris51-prod.benefitdigital.com.cn",
      //      "Authorization" -> "Basic YmVuZWZpdDpoeWJyaXM=",
      //      "Cookie" -> "symfony=apmod5halbtlj89pps8a3vi5t6; 38tERD42XE=%7B%22cart%22%3A%5B%5D%2C%22shoppingCartNumberOfItems%22%3A0%2C%22user%22%3Anull%2C%22shoppingCartId%22%3A%22anonymous%22%2C%22customerCountry%22%3A%22CN%22%7D",
      "User-Agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.94 Safari/537.36"
    )


  val excludes = Set[String]()

  val domainConstraints = Set("http://help-en-cn.nike.com", "http://help-zh-cn.nike.com", "http://help-ja-jp.nike.com")

  val urlBank = context.actorOf(Props(classOf[UrlBank], excludes, domainConstraints, 100), "UrlBank")

  urlBank ! Deposit(PageRequest(headers, initialUrl, initialName, None, 0))

  val dispatcher = context.actorOf(HttpRequestDispatcher.props(urlBank), HttpRequestDispatcher.name)
  context.watch(urlBank)

  val httpErrorRecorder = context.actorOf(HttpErrorRecorder.props(), HttpErrorRecorder.name)

  val parserLead = context.actorOf(Props(classOf[ParserLead], urlBank), ParserLead.name)

  val imageDownloadLead = context.actorOf(Props(classOf[ImgDownloadLead]), ImgDownloadLead.name)

  override def receive: Receive = LoggingReceive {
    case url: String => context.actorOf(HttpRequestDispatcher.props(urlBank))
    case Terminated(urlBank) =>
      AsyncWebClient.shutdown
      context.children foreach context.stop
      context.stop(self)
      context.system.shutdown
  }

}

object Main {
  val name = "Main"
  val path = "akka://" + name + "/user/app/"
}