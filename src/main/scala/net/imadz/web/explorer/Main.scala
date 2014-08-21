package net.imadz.web.explorer

import akka.actor.{Actor, ActorLogging, Terminated}
import akka.event.LoggingReceive

/**
 * Created by geek on 8/20/14.
 */
class Main extends Actor with ActorLogging {

  //  val initialUrl = "http://www.633qq.com/html/se/72116.html";
  //  val initialUrl = "http://hybris51-prod.benefitdigital.com.cn/";
   val initialUrl = "http://www.nike.com/";

  def headers = Map[String, String](
    //    "Cookie" ->
    //      "geoloc=cc=TW,rc=,tp=vhigh,tz=GMT+8,la=25.02,lo=121.45,bw=5000; AnalysisUserId=63.233.61.198.1408537457120468; guidS=be8ed134-0ad1-465d-f861-0ce09de609cb; guidU=4dd0f98d-2fe0-4e8b-ed21-64ff7194c05e; NIKE_COMMERCE_CCR=1408537476712; NIKE_COMMERCE_LANG_LOCALE=zh_CN; NIKE_COMMERCE_COUNTRY=CN; NIKE_CCR=8|CN|CN|TW|F|null|2|zh_CN|K|F; CONSUMERCHOICE_SESSION=t; CONSUMERCHOICE=cn/zh_cn; _smtz=smt_md%3D(direct)%26smt_pl%3D(none)%26smt_cp%3D(direct); _jzqckmp=1; RES_TRACKINGID=16175706844315580; ResonanceSegment=1; APID=8A1197D370D50CC6266499EDC90213B8.sin-323-app-ap-0; _jzqx=1.1408585064.1408585064.1.jzqsr=store%2Enike%2Ecom|jzqct=/.-; llCheck=9QREApmkwGakh3pG4JAHo9lwImUcjrJOyUeSFip/hTPHLFaVV/WSzbd4YLZzWW+kbWiUCRVaijU2a1MARWariRdVnCGjQkMUWubvX+RJ2fMOiUhGL4nnq+arNfOZ1gNQ; pr_id=11633220811; nike_locale=cn/zh_cn; sls=3; utag_main=_st:1408591364561$ses_id:1408585245303%3Bexp-session; mt.v=2.557715399.1408537467189; mt.mbsh=%7B%22calibration-china-b%22:1%7D; mt.m=%7B%22membership%22:%5B%22calibration-china-b%22%5D%7D; cartSummary=0%24%24undefined%24%241%24%2411633220811%24%244%24%24undefined%24%24DEFAULT_USER%24%240%24%24undefined; pr_data=MSQk5b635YGlIOmSnyQkMjAxNC8wOC8yMC83MzA3OTAxZi1lYWJjLTQzOTUtYTJmOC1hZWUxYmRhN2Q2OGEkJDIwMTQtMDktMjBUMDI6MDE6NTEuOTc3WiQkMTE2MzMyMjA4MTEkJA..; s_pers=%20s_fid%3D013B178BF70667E5-27C6B25DC4C7DFB1%7C1471747970221%3B%20c5%3Dnikecom%253Ehomepage%7C1408591370226%3B%20c6%3Dhomepage%7C1408591370229%3B; s_sess=%20s_ppv%3D%3B%20nike_referrer%3Dhttp%253A%252F%252Fhelp-zh-cn.swoosh.com%252Fapp%252Ferror%252Ferror_id%252F93%3B%20s_cc%3Dtrue%3B%20c51%3Dhorizontal%3B%20v41%3Dbrowse%253Anavigation%3B%20s_sq%3D%3B; s_vi=[CS]v1|29FA49C68501201F-40000105E0004FFE[CE]; Hm_lvt_ed406c6497cc3917d06fd572612b4bba=1408537498,1408586072; Hm_lpvt_ed406c6497cc3917d06fd572612b4bba=1408589575; _gscu_207448657=08537498g7ihln12; _gscs_207448657=t085850644oqwa710|pv:10; _gscbrs_207448657=1; _qzja=1.855299287.1408537500363.1408544026013.1408586072160.1408588563947.1408589574844.0.0.0.11.3; _qzjb=1.1408586072159.9.0.0.0; _qzjc=1; _qzjto=9.1.0; _jzqa=1.717398716513162100.1408537500.1408551169.1408585064.4; _jzqc=1; _jzqb=1.10.10.1408585064.1; _smta=53f4939a.22abe14d%2C1408586071%2C1408591378%2C9%2C3%2C0%2C1408544007; _smtp=a3aae4bd5bb5; _smtt=1408589578; RT=sl=1&ss=1408544001530&tt=11284&obo=0&bcn=%2F%2F36fb607e.mpstat.us%2F&dm=nike.com&si=8c7cf135-397e-4b33-a548-c6f37c760656&r=http%3A%2F%2Fwww.nike.com%2Fcn%2Fzh_cn%2F&ul=1408589585696",
    "User-Agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.76 Safari/537.36"
  )


  val excludes = Set[String](
    "http://www.ping.com",
    "http://help-zh-cn.swoosh.com",
    "http://nikeplus.nike.com"
  )

  val domainConstraints = Set("hybris51-prod.benefitdigital.com.cn", "nike.com")

  val dispatcher = context.actorOf(HttpRequestDispatcher.props(headers, excludes, initialUrl, domainConstraints, 100), "HttpRequestDispatcher")
  context.watch(dispatcher)

  val httpErrorRecorder = context.actorOf(HttpErrorRecorder.props(), HttpErrorRecorder.name)

  override def receive: Receive = LoggingReceive {
    case url: String => context.actorOf(HttpRequestDispatcher.props(headers, excludes, url, domainConstraints, 10))
    case Terminated(dispatcherActor) =>
      context.children foreach context.stop
      context.stop(self)
      context.system.shutdown
      AsyncWebClient.shutdown
  }


}
