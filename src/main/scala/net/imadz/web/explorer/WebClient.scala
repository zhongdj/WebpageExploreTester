package net.imadz.web.explorer

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}
import java.util.concurrent._

import akka.actor.ActorRef
import net.imadz.web.explorer.utils.LinkUtils

import scala.concurrent.{ExecutionContext, Future}

trait WebClient {
  def get(headers: Map[String, String])(url: String)(implicit exec: Executor): Future[String]
}

case class BadStatus(status: Int) extends RuntimeException

object AsyncWebClient {

  lazy val pageGetThreadPool: ExecutorService = Executors.newFixedThreadPool(10, new ThreadFactory {
    var counter = 0;

    override def newThread(r: Runnable): Thread = {
      counter += 1
      new Thread(r, "AsyncWebClient-Pool-" + counter)
    }
  })
  implicit val exec: ExecutionContext = {
    ExecutionContext.fromExecutor(pageGetThreadPool)
  }

  def get(headers: Map[String, String])(url: String)(dispatcher: ActorRef, pageRequest: PageRequest): Future[String] = {

    Future {
      var conn: HttpURLConnection = null
      var reader: BufferedReader = null
      try {
        conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
        conn.setReadTimeout(60000)
        conn.setDoInput(true)
        conn.setInstanceFollowRedirects(false)
        headers.foreach { case (k: String, v: String) => conn.setRequestProperty(k, v)}
        if (conn.getResponseCode >= 400) {
          throw new BadStatus(conn.getResponseCode)
        } else if (conn.getResponseCode >= 300) {
          val rawUrl = conn.getHeaderField("Location")
          val newUrl = LinkUtils.absoluteUrl(rawUrl, url)
          println("Response Code: " + conn.getResponseCode)
          println("Redirect Found on url: " + url)
          println("raw Url is: " + rawUrl)
          println("new Url is: " + newUrl)
          if (null != newUrl) {
            dispatcher ! PageRequest(headers, newUrl, "RedirectPage", Some(pageRequest), pageRequest.depth + 1)
            ""
          } else {
            throw new BadStatus(conn.getResponseCode)
          }
        } else {
          reader = new BufferedReader(new InputStreamReader(conn.getInputStream))
          var line = reader.readLine
          val resultBuilder = new StringBuilder(line)

          while (line != null) {
            line = reader.readLine
            resultBuilder.append("\n").append(line)
          }
          resultBuilder.toString
        }
      } finally {
        if (null != reader) reader.close
        conn.asInstanceOf[HttpURLConnection].disconnect
      }
    }
  }


  lazy val imageGetThreadPool: ExecutorService = Executors.newFixedThreadPool(5, new ThreadFactory {
    var counter = 0;

    override def newThread(r: Runnable): Thread = {
      counter += 1
      new Thread(r, "ImageHeaders-Pool-" + counter)
    }
  })

  val imageHeaderExec: ExecutionContext = {
    ExecutionContext.fromExecutor(imageGetThreadPool)
  }

  def connectOnly(headers: Map[String, String])(url: String): Future[Int] = Future {
    var conn: HttpURLConnection = null
    try {
      conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      conn.setReadTimeout(60000)
      conn.setDoInput(true)
      headers.foreach { case (k: String, v: String) => conn.setRequestProperty(k, v)}
      if (conn.getResponseCode < 400) {
        conn.getResponseCode
      } else {
        throw new BadStatus(conn.getResponseCode)
      }
    } finally {
      conn.asInstanceOf[HttpURLConnection].disconnect
    }
  }(imageHeaderExec)

  def shutdown(): Unit = {
    pageGetThreadPool.shutdown
    imageGetThreadPool.shutdown
  }

}

object test extends App {

  val url = "http://help-en-cn.nike.com/app/answers/detail/article/nike-jobs/snav/p/navt/About%20Nike%2C%20Inc./a_id/34193"
  val headers = Map.empty

  var conn: HttpURLConnection = null
  var reader: BufferedReader = null
  try {
    conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    conn.setReadTimeout(60000)
    conn.setDoInput(true)
    conn.setInstanceFollowRedirects(false)
    //headers.foreach { case (k: String, v: String) => conn.setRequestProperty(k, v)}
    if (conn.getResponseCode >= 400) {
      throw new BadStatus(conn.getResponseCode)
    } else if (conn.getResponseCode == 301 || conn.getResponseCode == 302) {
      val newUrl = conn.getHeaderField("Location")
      if (null != newUrl) {
        //dispatcher ! PageRequest(headers, newUrl, "RedirectPage", Some(pageRequest), pageRequest.depth + 1)
        ""
      } else {
        throw new BadStatus(conn.getResponseCode)
      }
    } else {
      reader = new BufferedReader(new InputStreamReader(conn.getInputStream))
      var line = reader.readLine
      val resultBuilder = new StringBuilder(line)

      while (line != null) {
        line = reader.readLine
        resultBuilder.append("\n").append(line)
      }
      resultBuilder.toString
    }
  } finally {
    if (null != reader) reader.close
    conn.asInstanceOf[HttpURLConnection].disconnect
  }
}