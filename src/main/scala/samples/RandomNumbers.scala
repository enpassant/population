/**
 *
 */

package samples

import scala.util.Random

import ga.Chromosoma
import ga.Population

/**
 * @author kalman
 *
 */
object RandomNumbers extends Population {

  val random = new Random

  val length = 12
  val destChrom = createChromosoma(length)
  println(destChrom)

  override def generateRandomGenome(index: Int, random: Random) = random.nextInt(1000)

  def evaluate(chromosoma: Chromosoma) = {
	if (random.nextDouble < 0.0001) {
	  throw new NullPointerException("Error")
	}

	val sum = (chromosoma.genome.view.zipWithIndex foldLeft 0) {
	  case (acc, (value, index)) =>
      	val dif = value - destChrom.genome(index)
      	acc + dif * dif
	}

	Chromosoma(chromosoma.genome, -math.sqrt(sum), chromosoma.user)
  }
}
