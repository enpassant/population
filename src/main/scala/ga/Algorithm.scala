/**
 *
 */
package ga

import scala.util.Random
import akka.actor.Actor
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.routing.FromConfig
import akka.actor.ActorSystem
import akka.actor.ActorRef

case class Generate(collector: ActorRef, length: Int)
case class Cross(collector: ActorRef, chr1: Chromosoma, chr2: Chromosoma)
case class Add(algorithm: ActorRef, chromosoma: Chromosoma)
case class Exit
case class Show
case class Run
case class Change(chromosomas: OrderedArrayBuffer[Chromosoma])

/**
 * @author kalman
 *
 */
class Algorithm(val size: Int, val length: Int, val count: Int, val population: Population) extends Actor {
  import akka.actor.OneForOneStrategy
  import akka.actor.SupervisorStrategy._
  import scala.concurrent.duration._

  val random = new Random

  // Create the 'collector' actor
  val collector = context.actorOf(Props(new Collector(size)), "collector")

  // Create the 'generator' actor
//  val generator = context.actorOf(Props(new Generator(population)).withRouter(FromConfig), "generator")

  def getDRandom(N: Int): Int = {
    ((Math.pow(random.nextDouble / 2 + 0.5, 2) - 0.25) * N - 0.001).toInt
  }

  override val supervisorStrategy =
  	OneForOneStrategy(maxNrOfRetries = -1, withinTimeRange = 10 minute) {
//  		case _: ArithmeticException => Escalate
  		case _: NullPointerException => Restart
//  		case _: IllegalArgumentException => Stop
  		case _: Exception => Resume
  }

  def init = {
    collector ! Run
    for (i <- 1 to size) {
      val generator = context.actorOf(Props(new Generator(population)))
      generator ! Generate(collector, length)
    }
  }

  def makeStep(chromosomas: OrderedArrayBuffer[Chromosoma]) = {
    collector ! Run
    for (j <- 1 to size) {
      val chrom1 = chromosomas(getDRandom(chromosomas.length))
      val chrom2 = chromosomas(getDRandom(chromosomas.length))
      val generator = context.actorOf(Props(new Generator(population)))
      generator ! Cross(collector, chrom1, chrom2)
    }
  }

  init

  def receive = active(1, null, new OrderedArrayBuffer[Chromosoma])

  def active(step: Int, best: Chromosoma, chromosomas: OrderedArrayBuffer[Chromosoma]): Receive = {

    case Change(chromosomas) =>
      population.show(step, chromosomas.head, chromosomas)

      if (population.exit(chromosomas) || step >= count) self ! Exit
      else makeStep(chromosomas)

      context become active(step + 1,
    	  if (best == null || chromosomas.head.fitness > best.fitness) {
    		chromosomas.head
    	  } else best,
          chromosomas)

    case Show => population.show(step, best, chromosomas)

    case Exit =>
      population.show(step, best, chromosomas)
      context.stop(self)
  }
}
