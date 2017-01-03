/**
 *
 */
package ga

import scala.util.Random

import akka.actor.Actor
import akka.actor.actorRef2Scala
import ga._

/**
 * @author kalman
 *
 */
class Generator(val population: Population) extends Actor {

  val random = new Random

  override def preRestart(reason: Throwable, message: Option[Any]) = {
    message match {
      case Some(Generate(collector, length))
        => self.tell(message.get, sender)
      case Some(Cross(collector, chrom1, chrom2))
        => self.tell(Generate(collector, chrom1.genome.length), sender)
      case any => println(any)
    }
  }

  def crossChromosoma(chrom1: Chromosoma, chrom2: Chromosoma): Chromosoma = {
    val length = chrom1.genome.length

    val (genome, _) = (0 until length foldLeft (IndexedSeq[Int](), chrom1)) {
      case (acc, index) =>
        val chrom = if (random.nextDouble <= population.pCrossOver) {
          if (acc._2 == chrom1) chrom2 else chrom1
        } else acc._2

        val value = if (random.nextDouble <= population.pMutate) {
          population.generateRandomGenome(index, random)
        } else {
          chrom.genome(index)
        }
        (acc._1 :+ value, chrom)
    }
    Chromosoma(genome, 0.0, None)
  }

  def receive = {
    case Generate(collector, length) =>
      collector ! Add(sender, population.evaluate(
        population.createChromosoma(length) ) )
      context.stop(self)

    case Cross(collector, chrom1, chrom2) =>
      if (random.nextDouble <= population.pReplicate) {
				collector ! Add(sender, chrom1)
				context.stop(self)
      } else {
				collector ! Add(sender, population.evaluate(
					crossChromosoma(chrom1, chrom2) )
				)
				context.stop(self)
      }
  }
}
