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
      if(message.startsWith("/")) {
        if(message.startsWith("/GoOnline")) {
          context.parent ! GoOnline
        } else if(message.startsWith("/GoOffline")) {
          context.parent ! GoOffline
        } else if(message.startsWith("/name")) {
          val tokens = message.split(" ")
          if(tokens.length > 1) {
            println("Username " + name + " is renamed as " + tokens(1))
            name = tokens(1)
          }
        } else if(message.startsWith("/AddChatParticipant")) {
          context.parent ! AddChatParticipant
        } else if(message.startsWith("/RemoveChatParticipant")) {
          val tokens = message.split(" ")
          if(tokens.length > 1) {
            val id = Integer.parseInt(tokens(1))
            context.parent ! RemoveChatParticipant(id)
          }          
        }
      } else if(message.length() > 0) {
        context.parent ! Speak(name, message)
      }
      consoleActor ! EnableConsole(name)
      
    case Begin =>
      log.debug("UserActor: Begin")
      consoleActor ! EnableConsole(name)
      
    case Reply(senderName: String, message: String) =>
      log.debug("UserActor: Reply")
      // Dump to console
      println(senderName + " : " + message)
  }
}