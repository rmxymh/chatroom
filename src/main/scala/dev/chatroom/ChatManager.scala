package dev.chatroom

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.event.Logging

case object SetupSystem
case object Begin
case class Reply(name: String, message: String)
case class Speak(name: String, message: String)
case object Shutdown
case object GoOnline
case object GoOffline

class ChatManager extends Actor {
  
  val log = Logging(context.system, this)

  val maxParticipant = 3
  var chatParticipants = List[ActorRef]()
  var userActor: ActorRef = null 
  
  def receive = {
    case SetupSystem => 
      log.debug("* ChatManager: SetupSystem")
      for( x <- 1 to maxParticipant ) {
        log.debug("** ChatManager: Setup ChatParticipant " + x.toString())
        val bootname = "%Participant%(" + x.toString() + ")"
        val actorname = "Participant" + x.toString()
        chatParticipants :::= List(context.actorOf(Props(new ChatParticipant(bootname)), name=actorname))
      }
      log.debug("** ChatManager: Setup User Actor")
      userActor = context.actorOf(Props[UserActor], name="userActor")
      userActor ! Begin
      
    case Speak(name: String, message: String) => 
      log.debug("** ChatManager: Speak(" + name + "): " + message)
      if(name.startsWith("%Participant%")) {
        if(userActor != null) {
          userActor ! Reply(name, message)
        }
      } else {
        chatParticipants.foreach { x => {
            x ! Speak(name, message)
          }
        }
      }
    case Reply(name: String, message: String) => 
      log.debug("** ChatManager: Reply")
      if(name.startsWith("%Participant%")) {
        if(userActor != null) {
          userActor ! Reply(name, message)
        }
      } else {
        chatParticipants.foreach { x => x ! Speak(name, message) }
      }
      
    case GoOnline => 
      log.debug("** ChatManager: GoOnline")
    case GoOffline => 
      log.debug("** ChatManager: GoOffline")
    case Shutdown => 
      log.debug("** ChatManager: Shutdown")
  }
}