package net.imadz.web.explorer

import akka.actor.Actor
import akka.actor.Actor.Receive

/**
 * Created by geek on 8/20/14.
 */
class Main extends Actor {

  val initialUrl = "http://www.nike.com/cn/zh_cn/";

  def headers = {
    Map[String, String](
      "Accept" -> "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
      "Accept-Encoding" -> "gzip,deflate,sdch",
      "Accept-Language" -> "en-US,en;q=0.8,zh-CN;q=0.6",
      "Cache-Control" -> "max-age=0",
      "Connection" -> "keep-alive",
      "Cookie" -> "geoloc=cc=TW,rc=,tp=vhigh,tz=GMT+8,la=25.02,lo=121.45,bw=5000; AnalysisUserId=63.233.61.198.1408537457120468; guidS=be8ed134-0ad1-465d-f861-0ce09de609cb; guidU=4dd0f98d-2fe0-4e8b-ed21-64ff7194c05e; NIKE_COMMERCE_CCR=1408537476712; nike_locale=cn/zh_cn; utag_main=_st:1408539276752$ses_id:1408537845493%3Bexp-session; mt.v=2.557715399.1408537467189; s_pers=%20s_fid%3D013B178BF70667E5-27C6B25DC4C7DFB1%7C1471695883106%3B%20c5%3Dnikecom%253Ehomepage%7C1408539283117%3B%20c6%3Dhomepage%7C1408539283125%3B; s_sess=%20s_cc%3Dtrue%3B%20c51%3Dhorizontal%3B%20v41%3Dbrowse%3B%20s_sq%3D%3B; APID=C3FB22C3CC5D8C3D6336B318971C98C1.sin-323-app-ap-0; NIKE_COMMERCE_LANG_LOCALE=zh_CN; NIKE_COMMERCE_COUNTRY=CN; CONSUMERCHOICE=CN/zh_CN; CONSUMERCHOICE_SESSION=t; NIKE_CCR=8|CN|CN|TW|F|null|2|zh_CN|K|F; cartSummary=0%24%24undefined%24%240%24%2411653042432%24%240%24%24undefined%24%24DEFAULT_USER%24%240%24%24undefined; pr_data=0; mt.mbsh=%7B%22calibration-china-b%22:1%7D; mt.m=%7B%22membership%22:%5B%22calibration-china-b%22%5D%7D; s_vi=[CS]v1|29FA49C68501201F-40000105E0004FFE[CE]; RT=sl=1&ss=1408537467636&tt=17675&obo=0&bcn=%2F%2F36fb619d.mpstat.us%2F&dm=nike.com&si=8c7cf135-397e-4b33-a548-c6f37c760656&r=http%3A%2F%2Fwww.nike.com%2Fcn%2Fzh_cn%2F&ul=1408537486244",
      "Host" -> "www.nike.com",
      "User-Agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.76 Safari/537.36"
    )
  }

  val excludes = Set[String]()

  context.actorOf(HttpRequestDispatcher.props(headers, excludes, initialUrl, 10))

  override def receive: Receive = {
    case _ =>
  }
}
