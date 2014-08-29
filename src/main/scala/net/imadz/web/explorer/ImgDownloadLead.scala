package net.imadz.web.explorer

import akka.actor.{Actor, ActorRef, ReceiveTimeout}
import akka.event.LoggingReceive
import net.imadz.web.explorer.ImgDownloadLead.ImgDownloadRequest
import net.imadz.web.explorer.ImgDownloader.ImgUrl
import net.imadz.web.explorer.UrlBank.Deposit

import scala.concurrent.duration._

/**
 * Created by geek on 8/24/14.
 */
class ImgDownloadLead(urlBank: ActorRef) extends Actor {
  override def receive: Receive = LoggingReceive {
    case ImgDownloadRequest(url, pageRequest) =>
      ImgDownloadLead.imageCount += 1
      context.actorOf(ImgDownloader.propsOfImages, "ImageDownloader-" + ImgDownloadLead.imageCount) ! ImgUrl(url)
    case Shutdown if context.children.isEmpty => context stop self
    case Shutdown =>
      context.setReceiveTimeout(10 seconds)
      context become shuttingDown
  }

  def shuttingDown: Receive = {
    case ImgDownloadRequest(url, pageRequest) =>
      urlBank ! Deposit(List(ImageRequest(pageRequest.headers, url, "Image got no name", pageRequest, pageRequest.depth + 1)))
    case ReceiveTimeout =>
      context stop self
  }
}


object ImgDownloadLead {
  var imageCount: Int = 0
  val name = "ImgDownloadLead"
  val path = Main.path + name

  case class ImgDownloadRequest(url: String, p: PageRequest)

}
