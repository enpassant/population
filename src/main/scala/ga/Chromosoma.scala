package ga

class Chromosoma(val length: Int) extends Ordered[Chromosoma] {
  val genome = new Array[Int](length)
  var fitness = 0.0 
  var user: Any = _

  def compare(that: Chromosoma) =
    this.fitness.compare(that.fitness)

  override def toString(): String = genome.mkString(", ") + " >> Fitness: " + fitness
}

object Chromosoma {
  def apply(length: Int) = new Chromosoma(length)
}