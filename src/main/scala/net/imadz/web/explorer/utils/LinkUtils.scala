package net.imadz.web.explorer.utils

import net.imadz.web.explorer.{HttpRequest, ImageRequest, PageRequest}

import scala.util.matching.Regex

/**
 * Created by geek on 8/21/14.
 */
object LinkUtils {

  private val IMG_TAG = "(?s)(?i)<img ([^<]+)>".r
  private val IMG_SRC = """\s*(?i)src\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+))\s*""".r
  private val ANCHOR_GREEDY_TAG = """(?s)(?i)<a (.*)>(.+)?</a>""".r
  private val ANCHOR_NON_GREEDY_TAG = """(?s)(?i)<a (.+?)>(.+?|)</a>""".r
  private val HREF_ATTR = """(?s)\s*(?i)href\s*=\s*\\?(?:"([^"\\]*)\\?"|'([^'\\]*)\\?'|([^'">\s]+))\s*""".r
  private val COMP_TAG = """(?s)(?i)(?:.*?<span>(.*)?</span>.*|(.*))""".r

  //"""(?s)(?i)(?:[^>]*>([^<]*)<.*|(.*))""".r

  private val invalidChars = '|' :: '\\' :: '\"' :: '\'' :: '{' :: '[' :: Nil

  def findLinks(htmlContent: String, httpRequest: PageRequest, withImageLink: Boolean = false): List[HttpRequest] = {

    val pages: List[PageRequest] = findPageLinkWithName(htmlContent) filter { case (rawUrl, _) =>
      isValidUrl(rawUrl)
    } map { case (rawUrl, name) =>
      absoluteUrl(rawUrl, httpRequest.url) -> name
    } map { case (rawUrl, name) =>
      trancateInavlidChars(rawUrl) -> name
    } map { case (rawUrl, name) =>
      PageRequest(httpRequest.headers, rawUrl, name, Some(httpRequest), httpRequest.depth + 1)
    } toList

    if (withImageLink) {
      val images: List[ImageRequest] = findImageLinks(htmlContent) filter (isValidUrl) map { rawUrl =>
        absoluteUrl(rawUrl, httpRequest.url)
      } map { rawUrl =>
        trancateInavlidChars(rawUrl)
      } map { url =>
        ImageRequest(httpRequest.headers, url, "image_no_names", httpRequest.asInstanceOf[PageRequest], httpRequest.depth + 1)
      } toList

      pages ::: images
    } else {
      pages
    }
  }

  private def isValidUrl: (String) => Boolean = {
    case rawUrl =>
      rawUrl.size > 0 && !rawUrl.trim.startsWith("#") && !rawUrl.contains("javascript") && !rawUrl.startsWith("mailto:")
  }


  def findPageLinkWithName(body: String): List[(String, String)] = {
    for {
      ANCHOR_NON_GREEDY_TAG(anchorTag, anchorTextValue) <- ANCHOR_NON_GREEDY_TAG findAllIn body
      key <- extractUrl(anchorTag)
    } yield {
      (key, extractLinkName(anchorTextValue))
    }
  } toList

  private def extractUrl(anchorTagWithAttrs: String): Option[String] = {

    val result = {
      for {
        HREF_ATTR(dquot, quot, bare) <- HREF_ATTR findAllIn anchorTagWithAttrs
      } yield {
        if (dquot != null) dquot.trim
        else if (quot != null) quot.trim
        else bare.trim
      }
    } toList

    if (!result.isEmpty) Some(result.head)
    else None
  }

  private def extractLinkName(linkHtml: String): String = if (linkHtml contains "<") nameInsideLinkHtml(linkHtml) else linkHtml


  def absoluteUrl(rawUrl: String, contextUrl: String): String = {
    val domainRegex = new Regex( """(http|https)://.*?/""")
    val domainUrl = domainRegex.findFirstIn(contextUrl).getOrElse(contextUrl + "/")
    if (rawUrl.startsWith("http") || rawUrl.startsWith("https")) rawUrl
    else if (rawUrl.startsWith("//")) {
      "http:" + rawUrl
    } else if (rawUrl.startsWith("/")) {
      if (domainUrl.endsWith("/"))
        domainUrl.take(domainUrl.length - 1) + rawUrl
      else
        domainUrl + rawUrl
    } else {
      val prefixUrl = new Regex( """(http|https)://.*/""").findFirstIn(contextUrl).getOrElse(contextUrl)
      prefixUrl + rawUrl
    }
  }

  def nameInsideLinkHtml(text: String) = {
    for (m <- """<[^>]+>([^<]+)<""".r.findAllMatchIn(text)) yield m.group(m.groupCount)
  }.mkString(" ")

  private def trancateInavlidChars(rawUrl: String): String = {
    def exists: (Int) => Boolean = _ > 0
    def position: (Char) => Int = rawUrl.indexOf(_)
    lazy val minInvalidPosition = {
      val invalidPositions = (invalidChars map position filter exists)
      invalidPositions.foldLeft(Int.MaxValue) { (min, index) => Math.min(min, index)}
    }
    if (minInvalidPosition < rawUrl.length) rawUrl.substring(0, minInvalidPosition)
    else rawUrl
  }


  private def findImageLinks(body: String): Iterator[String] = {
    for {
      IMG_TAG(anchor) <- IMG_TAG.findAllIn(body)
      IMG_SRC(dquot, quot, bare) <- IMG_SRC findAllIn (anchor)
    } yield if (dquot != null) dquot trim
    else if (quot != null) quot trim
    else bare trim
  }
}
