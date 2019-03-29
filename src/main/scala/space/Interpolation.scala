package space

import Implicits._

trait Interpolation extends InnerProduct {
  val resolution = 64

  def interpolate(cs: Vector[Concept]): Vector[Concept]
}

trait LinearFill extends Interpolation {

  /*
  Linear Interpolation:
  - Each part is of equal length
  - Sum of length of the parts equals the length of the whole
  - Each part (and whole) has the same angle with respect to any given vector
   */

  // assumes Euclidian inner product
  def interpolate(cs: Vector[Concept]): Vector[Concept] = {
    val K = resolution / cs.length // assume resolution > cs.length
    (cs :+ cs.head).sliding(2)
      .flatMap({ case Vector(Concept(a), Concept(b)) =>
        val delta = (a - b) / (K - 1)
        (0 until K).map(k => Concept(a + (delta * k)))
      }).toVector
  }
}

