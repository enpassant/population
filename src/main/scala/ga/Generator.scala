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

  val collector = context.actorFor("../../collector")

  val random = new Random
  
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
    case Generate(length) => 
      collector ! population.evaluate( population.createChromosoma(length) )

    case Cross(chrom1, chrom2) => 
      if (random.nextDouble <= population.pReplicate) {
    	collector ! chrom1
      } else {
    	collector ! population.evaluate( crossChromosoma(chrom1, chrom2) )
      }
  }
}
