package net.imadz.web.explorer

import java.io._
import java.net.{HttpURLConnection, URL}
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, Props}
import akka.event.LoggingReceive

import scala.util.Try

/**
 * Created by geek on 5/11/14.
 */
class ImgDownloader extends Actor {

  import net.imadz.web.explorer.ImgDownloader.{ImgUrl, foldSeq, imgRepo, imgSeq, sizeFilter}

  def receive = LoggingReceive {
    case ImgUrl(urlString) =>
      println(urlString)
      val url = new URL(urlString)
      val httpConnection = url.openConnection.asInstanceOf[HttpURLConnection]
      val length = httpConnection.getHeaderField("Content-Length")

      if (null == length || length.toInt > sizeFilter) {
        val writer = createWriter(fileExtension(urlString))
        val is = httpConnection.getInputStream

        Try {
          val buffer = new Array[Byte](64 * 1024)
          var length = 0
          length = is.read(buffer)
          while (length > 0) {
            writer.write(buffer, 0, length)
            length = is.read(buffer)
          }
        } match {
          case _ =>
            writer.close
            is.close
            httpConnection.disconnect
        }
      }

      context.stop(self)
  }

  def fileExtension(url: String) = {
    url.substring(url.lastIndexOf('.') + 1)
  }

  def createWriter(fileExtension: String) = {
    this.synchronized {
      if (imgSeq.intValue >= 2000) {
        foldSeq.incrementAndGet
        imgSeq.set(0)
      }
      val dir = new File(imgRepo + File.separator + foldSeq.get)
      if (!dir.exists) dir.mkdirs()
    }
    val imgFile = new File(imgRepo + File.separator + foldSeq.get + File.separator + imgSeq.getAndIncrement + "." + fileExtension)
    if (imgFile.exists) imgFile.delete
    else imgFile.createNewFile

    new FileOutputStream(imgFile)
  }
}

object ImgDownloader {
  val userHome = System.getProperty("user.home")
  val imgRepo = s"${userHome}" + File.separator + "imgRepo"
  val imgSeq = new AtomicInteger(2000)
  val foldSeq = new AtomicInteger(0)
  val sizeFilter: Int = 64 * 1024

  val downloaderSeq = new AtomicInteger(1)

  case class ImgUrl(val url: String)

  def propsOfImages = Props(classOf[ImgDownloader]).withDispatcher("images-download-dispatcher")

}