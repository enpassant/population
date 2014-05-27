import scala.util.Random

import akka.actor.ActorSystem
import akka.actor.Props

object Popu extends App {

  type Evaluate = (Chromosoma) => Chromosoma

  // Create the 'population' actor system
  val system = ActorSystem("population")
  
  // Create the 'population' actor
//  val algorithm = system.actorOf(Props(new Algorithm(10000, RandomNumbers.length, 100, RandomNumbers)), "population")

  // Create the 'population' actor
  val algorithm = system.actorOf(Props(new Algorithm(1200, TicTacToe.reducedTables.size, 120, TicTacToe)), "population")
}
