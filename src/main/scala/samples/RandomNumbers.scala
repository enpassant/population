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

  val length = 12
  val destChrom = createChromosoma(length)
  println(destChrom)

  override def generateRandomGenome(index: Int, random: Random) = random.nextInt(1000)

  def evaluate(chromosoma: Chromosoma) = {
    var sum = 0
    for (i <- 0 until chromosoma.length) {
      val dif = chromosoma.genome(i) - destChrom.genome(i)
      sum += dif * dif
    }
    chromosoma.fitness = -math.sqrt(sum)
    chromosoma
  }
}
