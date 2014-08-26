package net.imadz.web.explorer.utils

/**
 * Created by Scala on 14-8-26.
 */
import org.scalatest.Suite

class HrefRegexTestSuite extends Suite{


  def testAnchorWithNoOtherAttributes() {
    val simple = """<a href="/app/answers/detail/article/size-chart-men-tops-standard/snav/p/navt/Size%20Charts">Size charts</a>"""
    val expectedResult = "/app/answers/detail/article/size-chart-men-tops-standard/snav/p/navt/Size%20Charts"
    val actualResult = LinkUtils.findPageLinkWithName(simple)
    assert (!actualResult.isEmpty)
    assert (actualResult.size == 1)
    assert (expectedResult === actualResult.head._1)
  }

  def testAnchorWithBackSlash() {
    val simple =
      """
        |<a href=\"http://help-zh-cn.nike.com/app/answers/detail/article/terms\">使用条款</a>
      """.stripMargin
    val expectedResult = "http://help-zh-cn.nike.com/app/answers/detail/article/terms"
    val actualResult = LinkUtils.findPageLinkWithName(simple)
    assert (!actualResult.isEmpty)
    assert (actualResult.size == 1)
    assert (expectedResult === actualResult.head._1)
  }
  def testAnchorWithMultiAttributes() {
    val sample = """<li>
                          <a data-subnav-label="高尔夫"
                             class="nsg-text--medium-grey nsg-font-family--platform"
                             href="http://www.nike.com/cn/zh_cn/c/golf"

                             data-nav-tracking="golf"
                             data-track-click="true"
                             >
                              高尔夫
                          </a>
                      </li>

    """
    val expectedResult = "http://www.nike.com/cn/zh_cn/c/golf"
    val actualResult = LinkUtils.findPageLinkWithName(sample)
    assert (!actualResult.isEmpty)
    assert (actualResult.size == 1)
    assert (expectedResult === actualResult.head._1)
  }

  def testMultiAnchors() {
    val sample =
      """
        |<a href="http://www.nike.com/cn/zh_cn/c/men1"  data-nav-tracking="men" data-track-click="true" >
        |                <span class="nsg-font-family--platform nsg-button-font-size--small facet-label">
        |
        |                    Men1
        |
        |                  <span class="nsg-glyph--arrow-down nsg-text--medium-light-grey"></span>
        |                </span>
        |            </a>
        |           <a href="http://www.nike.com/cn/zh_cn/c/men2"  data-nav-tracking="men" data-track-click="true" >
        |                <span class="nsg-font-family--platform nsg-button-font-size--small facet-label">
        |
        |                    Men2
        |
        |                  <span class="nsg-glyph--arrow-down nsg-text--medium-light-grey"></span>
        |                </span>
        |            </a>
      """.stripMargin

    val expectedResult1 = "http://www.nike.com/cn/zh_cn/c/men1"
    val expectedResult2 = "http://www.nike.com/cn/zh_cn/c/men2"
    val actualResult = LinkUtils.findPageLinkWithName(sample)
    assert (!actualResult.isEmpty)
    assert (actualResult.size == 2)
    assert (expectedResult1 === actualResult.head._1)
    assert (expectedResult2 === actualResult.tail.head._1)
  }
}
