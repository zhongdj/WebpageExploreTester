package net.imadz.web

/**
 * Created by geek on 8/20/14.
 */
package object explorer {

  sealed abstract class HttpRequest() {

    def headers: Map[String, String]

    def url: String

    def name: String

    def previousRequest: Option[HttpRequest]

    def depth: Int

  }

  case class PageRequest(headers: Map[String, String], url: String, name: String, previousRequest: Option[HttpRequest], depth: Int) extends HttpRequest {


    override def canEqual(that: Any): Boolean = that.isInstanceOf[PageRequest]

    override def hashCode(): Int = url.hashCode()

    override def equals(obj: scala.Any): Boolean = if (canEqual(obj)) url.equals(obj.asInstanceOf[PageRequest].url) else false
  }

  case class ImageRequest(headers: Map[String, String], url: String, name: String, previousPageRequest: PageRequest, depth: Int) extends HttpRequest {

    override def previousRequest: Option[HttpRequest] = Some(previousPageRequest)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[ImageRequest]

    override def hashCode(): Int = url.hashCode()

    override def equals(obj: scala.Any): Boolean = if (canEqual(obj)) url.equals(obj.asInstanceOf[ImageRequest].url) else false
  }

}
