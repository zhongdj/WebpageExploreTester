package net.imadz.web

/**
 * Created by geek on 8/20/14.
 */
package object explorer {

  abstract class HttpRequest() {
    def url: String

    def previousRequest: Option[HttpRequest]

    def depth: Int
  }

  case class PageRequest(url: String, previousRequest: Option[HttpRequest], depth: Int) extends HttpRequest

  case class ImageRequest(url: String, previousPageRequest: PageRequest, depth: Int) extends HttpRequest {

    override def previousRequest: Option[HttpRequest] = Some(previousPageRequest)

  }

}