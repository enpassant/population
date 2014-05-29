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

case class Generate(length: Int)
case class Cross(chr1: Chromosoma, chr2: Chromosoma)
case class Exit
case class Show
case class Run
case class Change(chromosomas: OrderedArrayBuffer[Chromosoma])

/**
 * @author kalman
 *
 */
class Algorithm(val size: Int, val length: Int, val count: Int, val population: Population) extends Actor {
  val random = new Random

  // Create the 'crossMaker' actor
  val collector = context.actorOf(Props(new Collector(size)), "collector")

  // Create the 'evaluator' actor
  val generator = context.actorOf(Props(new Generator(population)).withRouter(FromConfig), "generator")

  def getDRandom(N: Int): Int = {
    ((Math.pow(random.nextDouble / 2 + 0.5, 2) - 0.25) * N - 0.001).toInt
  }

  def init = {
    collector ! Run
    for (i <- 1 to size) {
      generator ! Generate(length)
    }
  }
  
  def makeStep = {
    collector ! Run
    for (j <- 1 to size) {
      val chrom1 = population.chromosomas(getDRandom(population.chromosomas.length))
      val chrom2 = population.chromosomas(getDRandom(population.chromosomas.length))
      generator ! Cross(chrom1, chrom2)
    }
  }
  
  init

  def receive = {

    case Change(chromosomas) =>
      population.chromosomas = chromosomas
      population.step += 1
      if (population.best == null || population.chromosomas.head.fitness > population.best.fitness) {
        population.best = population.chromosomas.head
      }
      population.show(population.chromosomas.head)
      if (population.exit || population.step >= count) self ! Exit
      else makeStep

    case Show => population.show(population.chromosomas.head)

    case Exit =>
      population.show(population.best)
      context.stop(self)
  }
}
