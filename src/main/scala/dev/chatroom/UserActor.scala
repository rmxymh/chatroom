package dev.chatroom

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.event.Logging

case class MessageFromConsole(message: String)

class UserActor extends Actor {
  
  val log = Logging(context.system, this)
  
  val consoleActor = context.actorOf(Props[ConsoleActor], name="consoleActor")
  var name = "DefaultUser"
  
  def receive = {
    case MessageFromConsole(message: String) =>
      log.debug("UserActor: Message From Console")
      context.parent ! Speak(name, message)
      consoleActor ! EnableConsole
      
    case Begin =>
      log.debug("UserActor: Begin")
      consoleActor ! EnableConsole
      
    case Reply(senderName: String, message: String) =>
      log.debug("UserActor: Reply")
      // Dump to console
      println(senderName + " : " + message)
  }
}