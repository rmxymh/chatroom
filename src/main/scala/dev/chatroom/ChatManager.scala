package dev.chatroom

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Calendar

import akka.actor.Actor
import akka.actor.FSM
import akka.actor.ActorRef
import akka.actor.Props
import akka.event.Logging

sealed trait ChatroomState
case object Online extends ChatroomState
case object Offline extends ChatroomState

sealed trait ChatroomAction
case object GoOnline extends ChatroomAction
case object GoOffline extends ChatroomAction
case object SetupSystem extends ChatroomAction
case object Begin extends ChatroomAction
case class Reply(name: String, message: String) extends ChatroomAction
case class Speak(name: String, message: String) extends ChatroomAction
case object Shutdown extends ChatroomAction

class ChatManager extends Actor with FSM[ChatroomState, ChatroomAction] {
  
  //val log = Logging(context.system, this)
  val chatlogPath = "chat.log"
  var chatlog:BufferedWriter = null

  val maxParticipant = 3
  var chatParticipants = List[ActorRef]()
  var userActor: ActorRef = null 

  def initChatlog() {
    if(chatlog == null) {
      val file = new File(chatlogPath); 
      chatlog = new BufferedWriter(new FileWriter(file.getAbsoluteFile()))
      log.info("Write chat log at " + file.getAbsolutePath())
    }
  }
  
  def finalizeChatlog() {
    if(chatlog != null) {
      chatlog.close()
    }
  }
  
  def chat(name: String, message: String) {
    if(chatlog != null) {
      val time = Calendar.getInstance().getTime()
      val logMsg = time + " " + name + " > " + message
      chatlog.write(logMsg)
      chatlog.newLine()
      chatlog.flush()
    }
  }
  
  startWith(Offline, GoOffline)
  
  when(Offline) {
    case Event(SetupSystem, _) => 
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
      
      initChatlog
      goto(Online)
      
    case Event(GoOnline, _) => 
      log.debug("** ChatManager: GoOnline")
      initChatlog
      goto(Online)
      
    case Event(Shutdown, _) => 
      log.debug("** ChatManager: Shutdown")
      finalizeChatlog
      stay
      
    case Event(Speak(name: String, message: String), _) =>
      if(userActor != null) {
        userActor ! Reply("System", "Chatroom is offline.")
      }
      stay
  }
  

  when(Online) {      
    case Event(Speak(name: String, message: String), _) => 
      log.debug("** ChatManager: Speak(" + name + "): " + message)
      
      chat(name, message)
      
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
      stay
      
    case Event(Reply(name: String, message: String), _) => 
      log.debug("** ChatManager: Reply")
      chat(name, message)
      if(name.startsWith("%Participant%")) {
        if(userActor != null) {
          userActor ! Reply(name, message)
        }
      } else {
        chatParticipants.foreach { x => x ! Speak(name, message) }
      }
      stay
      
    case Event(GoOffline, _) => 
      log.debug("** ChatManager: GoOffline")
      finalizeChatlog
      goto(Offline)
      
    case Event(Shutdown, _) => 
      log.debug("** ChatManager: Shutdown")
      finalizeChatlog
      goto(Offline)
  }
  
  initialize()
}