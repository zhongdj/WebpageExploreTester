package net.imadz.web.explorer

import akka.actor.Actor
import net.imadz.web.explorer.ImgDownloadLead.ImgDownloadRequest
import net.imadz.web.explorer.ImgDownloader.ImgUrl

/**
 * Created by geek on 8/24/14.
 */
class ImgDownloadLead extends Actor {
  override def receive: Receive = {
    case ImgDownloadRequest(url, pageRequest) =>
      ImgDownloadLead.imageCount += 1
      context.actorOf(ImgDownloader.propsOfImages, "ImageDownloader-" + ImgDownloadLead.imageCount) ! ImgUrl(url)
  }
}

object ImgDownloadLead {
  var imageCount: Int = 0
  val name = "ImgDownloadLead"
  val path = "akka://Main/user/app/" + name

  case class ImgDownloadRequest(url: String, p: PageRequest)

}
