package dev.chatroom

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging

object SystemBoot {
  case object Boot
}

class SystemBoot extends Actor {

  val log = Logging(context.system, this)
  
  val chatManager = context.actorOf(Props[ChatManager], name="chatManager")
  
  def receive = {
    case SystemBoot.Boot =>
      log.debug("* Chatroom System Booting...")
      chatManager ! SetupSystem
  }
  
}