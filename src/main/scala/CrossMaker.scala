/**
 *
 */


import scala.util.Random

import akka.actor.Actor
import akka.actor.Props
import akka.actor.actorRef2Scala

/**
 * @author kalman
 *
 */
class CrossMaker(val evaluate: Chromosoma => Chromosoma) extends Actor {
  
  val collector = context.actorFor("/user/population/collector")

  def crossChromosoma(chrom1: Chromosoma, chrom2: Chromosoma): Chromosoma = {
	val random = new Random
    var chromDest = new Chromosoma(chrom1.length)
    for (i <- 0 until chrom1.length) {
      if (random.nextDouble <= 0.5) {
    	chromDest.genome(i) = chrom2.genome(i)
      } else {
    	chromDest.genome(i) = chrom1.genome(i)
      }
    }
    chromDest
  }
  
  def receive = {
    case Cross(chrom1, chrom2) => {
      collector ! evaluate( crossChromosoma(chrom1, chrom2) )
    }
  }
}