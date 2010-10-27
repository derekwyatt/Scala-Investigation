
import scala.actors.Actor
import Actor._

case object GetMessages
case object Stop
case object UnknownThing
case class Messages(msg: List[String])
case class Remove(who: Listener)
case class Add(who: Listener)

object ChatServer3 extends Actor {
    private var chats: List[String] = Nil
    private var listeners: List[Listener] = Nil

    def act = loop {
        react(calcReact)
    }

    private def calcReact = {
        val handle: PartialFunction[Any, Unit] = {
            case s: String => {
                chats = chats ::: List(s)  
                notifyListeners()
            }
            case GetMessages => reply(Messages(chats))
            case Stop => reply(None); exit()
        }
        val mgt: PartialFunction[Any, Unit] = {
            if (chats.length < 3) {
                Map.empty
            } else {
                case Add(who) => {
                    listeners = who :: listeners
                    who ! Messages(chats)
                }
                case Remove(who) => listeners.filterNot(_ == who)
                case s => println("ChatServer3 received unknown message: [" + s + "]")
            }
        }
        handle orElse mgt
    }

    private def notifyListeners() {
        listeners.foreach(a => a ! Messages(chats))
    }

    this.start()
}

class Listener(val me: String) extends Actor { 
    def act {
        loop {
            react {
                case Messages(chats) => chats.indices.foreach(n => println(me + "[" + n + "]: " + chats(n)))
                case Stop => reply(None); exit()
            }
        }
    }
}

object Main {
    def main(args: Array[String]) {
        val one = new Listener("one");     one.start();   ChatServer3 ! Add(one)
        val two = new Listener("two");     two.start();   ChatServer3 ! Add(two)
        val three = new Listener("three"); three.start(); ChatServer3 ! Add(three)
        ChatServer3 ! "Hi!"
        ChatServer3 ! "Hey!"
        ChatServer3 ! "What's up dood?"
        ChatServer3 ! "Not much... just feeling a bit drunk"
        ChatServer3 ! "Cool.  I'm high."
        ChatServer3 ! UnknownThing
        ChatServer3 ! 12345

        ChatServer3 !? Stop
        one !? Stop
        two !? Stop
        three !? Stop
    }
}
