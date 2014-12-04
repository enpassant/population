/**
 *
 */
/**
 * @author kalman
 *
 */
package ga

import scala.util.Random

abstract class Population {
  def evaluate(chromosoma: Chromosoma): Chromosoma
  
  def createChromosoma(length: Int): Chromosoma = {
    val random = new Random
    val chromosoma = new Chromosoma(length)
    for (i <- 0 until length) chromosoma.genome(i) = generateRandomGenome(i, random)
    chromosoma
  }
  
  def generateRandomGenome(index: Int, random: Random): Int
  
  def pReplicate = 0.1
  
  def pMutate = 0.001
  
  def pCrossOver = 0.9
  
  def exit(chromosomas: OrderedArrayBuffer[Chromosoma]) = false
  
  def show(step: Int, best: Chromosoma, chromosomas: OrderedArrayBuffer[Chromosoma]) = {
    println(step + ". " + best)
  }

  def done(step: Int, best: Chromosoma, chromosomas: OrderedArrayBuffer[Chromosoma]) = show(step, best, chromosomas)
}
