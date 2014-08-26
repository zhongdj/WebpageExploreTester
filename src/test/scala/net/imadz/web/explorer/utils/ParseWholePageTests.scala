package net.imadz.web.explorer.utils

/**
 * Created by Scala on 14-8-26.
 */

import org.scalatest.Suite
import scala.io.Source

class ParseWholePageTests extends Suite {

  def testWholePage = {
    val actualResult = LinkUtils.findPageLinkWithName(importTestDataFromFile("/PageSourceCode"))
    actualResult foreach {
      x => println(x._1 + " -> " + x._2)
    }
  }

  def importTestDataFromFile(file: String): String = {
    Source.fromURL(getClass.getResource(file)).getLines().mkString("\n")
  }
}
