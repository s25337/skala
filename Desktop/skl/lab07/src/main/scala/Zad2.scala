import org.apache.pekko
import pekko.actor._

class Boss extends Actor {
  def receive: Receive = {
    case msg => println(s"Odebrałem wiadomość: $msg")
  }
}

@main 
def zad2: Unit = {

  def dane(): List[String] = {
   scala.io.Source.fromResource("ogniem_i_mieczem.txt").getLines.toList
  }
  val system = ActorSystem("WordCounter")
  val boss = system.actorOf(Props[Boss](), "boss")
  boss ! "Dzien dobry"

  println(dane())
}
