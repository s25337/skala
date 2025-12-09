import org.apache.pekko
import pekko.actor._


case class Start(nextPlayer: ActorRef)
case object Ping

class Player extends Actor {
  var nextPlayer: Option[ActorRef] = None
  var pingCount = 0

  def receive: Receive = {
    case Start(next) =>
      nextPlayer = Some(next)
      println(s"${self.path.name} started, sending to ${next.path.name}")
      next ! Ping
      
    case Ping =>
      pingCount += 1
      println(s"${self.path.name} received Ping #$pingCount from ${sender().path.name}")
      nextPlayer.foreach(_ ! Ping)
  }
}

object Zad1 extends App {

  println(" Dwoch playerow ")
  val system1 = ActorSystem("TwoPlayersSystem")
  val player1 = system1.actorOf(Props[Player](), "Player1")
  val player2 = system1.actorOf(Props[Player](), "Player2")
  
  player1 ! Start(player2)
  player2 ! Start(player1)
  
  Thread.sleep(5000) //5s
  system1.terminate()
  Thread.sleep(1000)
  

}