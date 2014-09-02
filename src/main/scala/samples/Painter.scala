/**
 *
 */
package samples

import java.io.File
import scala.util.Random
import ga.Chromosoma
import ga.Population
import javax.imageio.ImageIO
import java.awt.GraphicsEnvironment
import java.awt.Transparency
import java.awt.image.BufferedImage
import java.awt.Color

/**
 * @author kalman
 *
 */
object Painter extends Population {

  val count = 50
  val itemCount = 8
  
  val graphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment
  val graphicsDevice = graphicsEnvironment.getDefaultScreenDevice
  val graphicsConfiguration = graphicsDevice.getDefaultConfiguration

  val image = ImageIO.read(new File("resource/image/wife.jpg"))
  val width = image.getWidth
  val height = image.getHeight
  
  override def generateRandomGenome(index: Int, random: Random) = index match {
    case 0 => random.nextInt(width)
    case 1 => random.nextInt(height)
    case 2 => random.nextInt(width)
    case 3 => random.nextInt(height)
    case _ => random.nextInt(256)
  }

  def createImage(chromosoma: Chromosoma) = {
    val newImage = graphicsConfiguration.createCompatibleImage(width, height, Transparency.BITMASK);
    val graphics = newImage.createGraphics
    
    for (i <- 0 until count) {
      val start = i * itemCount
      val color = new Color(chromosoma.genome(start + 4), chromosoma.genome(start + 5), 
          chromosoma.genome(start + 6), chromosoma.genome(start + 7))
      graphics.setColor(color);
      graphics.fillOval(chromosoma.genome(start + 0), chromosoma.genome(start + 1), 
          chromosoma.genome(start + 2), chromosoma.genome(start + 3))
    }
    
    graphics.dispose
    
    newImage
  }
  
  def evaluate(chromosoma: Chromosoma) = {
    val newImage = createImage(chromosoma)
    
    var sum = 0
    chromosoma.fitness = compareImages(newImage)
    chromosoma
  }
  
  def compareImages(newImage: BufferedImage) = {
    var diff = 0L;
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        val rgb1 = image.getRGB(i, j)
        val rgb2 = newImage.getRGB(i, j)
        val r1 = (rgb1 >> 16) & 0xff
        val g1 = (rgb1 >>  8) & 0xff
        val b1 = (rgb1      ) & 0xff
        val r2 = (rgb2 >> 16) & 0xff
        val g2 = (rgb2 >>  8) & 0xff
        val b2 = (rgb2      ) & 0xff
        diff += Math.abs(r1 - r2) * Math.abs(r1 - r2)
        diff += Math.abs(g1 - g2) * Math.abs(g1 - g2)
        diff += Math.abs(b1 - b2) * Math.abs(b1 - b2)
      }
    }
//    val n = width * height * 3.0
//    val p = 10000.0 - diff / n / 255.0
    -diff
  }
  
  override def done = {
    val newImage = createImage(best)
    val outputfile = new File("resource/image/saved.png");
    ImageIO.write(newImage, "png", outputfile);
  }

  override def show(chromosoma: Chromosoma) = {
    if (step % 10 == 0) done
    println(step + ". " + chromosoma.fitness + ", " + chromosomas.last.fitness)
  }
}