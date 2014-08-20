package net.imadz.web.explorer

import java.util.regex.{Pattern, Matcher}

import scala.concurrent.Future
import com.ning.http.client.AsyncHttpClient
import scala.concurrent.Promise
import java.util.concurrent.Executor

trait WebClient {
  def get(url: String)(implicit exec: Executor): Future[String]
}

case class BadStatus(status: Int) extends RuntimeException

object AsyncWebClient extends WebClient {

  private val client = new AsyncHttpClient

  def get(url: String)(implicit exec: Executor): Future[String] = {
    println(url)
    val f = client.prepareGet(url).execute();
    val p = Promise[String]()
    f.addListener(new Runnable {
      def run = {
        val response = f.get
        if (response.getStatusCode < 400)
          p.success(response.getResponseBodyExcerpt(131072))
        else p.failure(BadStatus(response.getStatusCode))
      }
    }, exec)
    p.future
  }

  def shutdown(): Unit = client.close()

}

object WebClientTest extends App {



}