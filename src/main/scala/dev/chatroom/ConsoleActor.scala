package dev.chatroom

import akka.actor.Actor
import akka.event.Logging

case object EnableConsole

class ConsoleActor extends Actor {
  
  val log = Logging(context.system, this)
  
  def receive = {
    case EnableConsole =>
      log.debug("ConsoleActor: EnableConsole")
      acceptUserInput
  }
  
  def acceptUserInput = {
    val msgs = io.Source.stdin.getLines.take(1)
    msgs.foreach { x => context.parent ! MessageFromConsole(x) }
  }
}