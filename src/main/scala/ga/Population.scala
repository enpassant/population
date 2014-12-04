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
    val genome = (0 until length).map {
      i => generateRandomGenome(i, random)
    } 
    Chromosoma(genome, 0.0, None)
  }
  
  def generateRandomGenome(index: Int, random: Random): Int
  
  def pReplicate = 0.1
  
  def pMutate = 0.001
  
  def pCrossOver = 0.3
  
  def exit(chromosomas: OrderedArrayBuffer[Chromosoma]) = false
  
  def show(step: Int, best: Chromosoma, chromosomas: OrderedArrayBuffer[Chromosoma]) = {
    println(step + ". " + best)
  }

  def done(step: Int, best: Chromosoma, chromosomas: OrderedArrayBuffer[Chromosoma]) = show(step, best, chromosomas)
}
