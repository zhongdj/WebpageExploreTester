package net.imadz.web.explorer

import akka.actor.{Props, ActorSystem}

/**
 * Created by Scala on 14-9-4.
 */
object LocalTest extends App {

  val engine = ActorSystem("spiderman").actorOf(Props(classOf[Main], None, None), Main.name)
  engine ! TestRun("http://www.nike.com", "", "", "nike", false, 15)
}
