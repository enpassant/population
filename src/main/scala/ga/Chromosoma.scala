package ga

class Chromosoma(val genome: IndexedSeq[Int], val fitness: Double, val user: Any) extends Ordered[Chromosoma] {
  def compare(that: Chromosoma) =
    this.fitness.compare(that.fitness)

  override def toString(): String = genome.mkString(", ") + " >> Fitness: " + fitness
}

object Chromosoma {
  def apply(genome: IndexedSeq[Int], fitness: Double, user: Any) = new Chromosoma(genome, fitness, user)
}