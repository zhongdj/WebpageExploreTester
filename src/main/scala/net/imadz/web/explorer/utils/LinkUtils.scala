package net.imadz.web.explorer.utils

/**
 * Created by geek on 8/21/14.
 */
object LinkUtils {

  val A_TAG = """(?s)(?i)<a (.*)>(.+)?</a>""".r
  val HREF_ATTR = """(?s)\s+(?i)href\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))\s*""".r

  def findPageLinks(body: String): Iterator[String] = {
    for {
      A_TAG(attrs, name) <- A_TAG findAllIn body
      HREF_ATTR(dquot, quot, bare) <- HREF_ATTR findAllIn attrs
    } yield if (dquot != null) dquot
    else if (quot != null) quot
    else bare
  }

}


object LinkUtilsTest extends App {

  val body =
    """
      |<div class="copy">
      |								<a class="sportwatch_link img_link" href="/plus/products/sport_watch/"></a>
      |								<div class="text">
      |									<h3><a class="sportwatch_link" href="/plus/products/sport_watch/">Nike+ SportWatch GPS<span class="arrow">B</span></a></h3>
      |									<p>適合想要進一步挑戰的積極跑者。使用 GPS 進行追蹤、記錄路段配速、提醒您跑步，還可以記下您的個人記錄。</p>
      |								</div>
      |							</div>
      |       <div class="faq">
      |								<a href="/plus/support#answers/detail/article/sportband-start" class="sprite"><span id="container"><span id="num">1. </span><span id="copy">Nike+ SportBand 入門指南</span></a></span>
      |							</div>
      |							<div class="faq">
      |								<a href="/plus/support#answers/detail/article/nikerunapp-getstarted" class="sprite"><span id="container"><span id="num">2. </span><span id="copy">Nike+ Running 應用程式入門指南</span></a></span>
      |							</div>
      |							<div class="faq">
      |								<a href="/plus/support#answers/detail/article/connect-install" class="sprite"><span id="container"><span id="num">3. </span><span id="copy">下載並安裝 Nike+ Connect</span></a></span>
      |							</div>
      |
      |        <div class="exp-darkbar" data-qa="darkbar.container">
      |            <div class="exp-darkbar-left-section">
      |                <div class="active"><span class="facet-label nsg-text--nike-orange nsg-button-font-size--xsmall nsg-font-family--platform" data-qa="darkbar.shop">购买</span></div>
      |                <div><a data-track-click="true" data-nav-tracking="nike plus" href="http://nikeplus.nike.com/plus/"><span class="facet-label nsg-text--nike-white nsg-button-font-size--xsmall nsg-font-family--platform" data-qa="darkbar.nikeplus">NIKE+</span></a></div>
      |            </div>
      |            <div class="exp-darkbar-right-section">
      |
      |                <div class="exp-default nsg-font-family--platform">
      |                  <a data-track-click="true" data-nav-tracking="email sign up"
      |                     data-path="http://store.nike.com//CN/zh_CN/?l=shop,email_signup"
      |                     href="http://store.nike.com//CN/zh_CN/?l=shop,email_signup"
      |                     data-qa="darkbar.email" class="exp-onenikenav-help">电子邮件注册
      |                  </a>
      |                </div>
      |    <li class="exp-help-dropdown-title nsg-font-family--platform"><a href="http://www.nike.com/cn/zh_cn/c/help"  data-track-click="true" data-nav-tracking="get help:get help">获取帮助</a></li>
      |
      |                                          <li >
      |                                              <a href="http://help-zh-cn.nike.com/app/answers/detail/article/returns-policy/"  data-track-click="true" data-nav-tracking="get help:returns">退换货</a>
      |                                          </li>
      |
      |                                          <li >
      |                                              <a href="https://secure-store.nike.com/cn/zh_cn/?l=shop,orderstatus"  data-track-click="true" data-nav-tracking="get help:order status">订单状态</a>
      |                                          </li>
      |
    """.stripMargin

  LinkUtils.findPageLinks( body ) foreach println
}