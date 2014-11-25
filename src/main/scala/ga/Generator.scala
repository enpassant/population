/**
 *
 */
package ga

import scala.util.Random

import akka.actor.Actor
import akka.actor.actorRef2Scala

/**
 * @author kalman
 *
 */
class Generator(val population: Population) extends Actor {

  val random = new Random
  
  override def preRestart(reason: Throwable, message: Option[Any]) {
    message match {
      case Some(Generate(collector, length)) => self.tell(message.get, sender)
      case Some(Cross(collector, chrom1, chrom2)) => self.tell(Generate(collector, chrom1.length), sender)
      case any => println(any)
    }
  }
  
  def crossChromosoma(chrom1: Chromosoma, chrom2: Chromosoma): Chromosoma = {
    var chromDest = new Chromosoma(chrom1.length)
    var chrom = chrom1
    for (i <- 0 until chrom1.length) {
      if (random.nextDouble <= population.pMutate) {
        // Mutation
    	chromDest.genome(i) = population.generateRandomGenome(i, random)
      } else {
        if (random.nextDouble <= population.pCrossOver) {
          chrom = if (chrom == chrom1) chrom2 else chrom1
        }
        chromDest.genome(i) = chrom.genome(i)
      }
    }
    chromDest
  }
  
  def receive = {
    case Generate(collector, length) => 
      collector ! Add(sender, population.evaluate( population.createChromosoma(length) ) )
      context.stop(self)

    case Cross(collector, chrom1, chrom2) => 
      if (random.nextDouble <= population.pReplicate) {
    	collector ! Add(sender, chrom1)
    	context.stop(self)
      } else {
    	collector ! Add(sender, population.evaluate( crossChromosoma(chrom1, chrom2) ) )
    	context.stop(self)
      }
  }
}
