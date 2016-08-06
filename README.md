Chatroom
========

A chatroom project implemented with akka with Scala.

## Dependency

The following dependency version are under development environment:

* Scala: 2.11.8
* akka: 2.4.8
* sbt: 0.13

## Usage

```bash
$ sbt compile
$ sbt run
```

## Tags

For different stage functions, you can find it via git tag.

### Level 2

#### Usage

* Add a new Chat Participant

```bash
(DefaultUser)# /AddChatParticipant
```

* Remove an existing Chat Participant
  * ID is an integer
  * If no such participant exists, no participant will be removed.

```bash
(DefaultUser)# /RemoveChatParticipant <ID>
```


