package space

import Implicits._

case class Dimension() {
  val space = new EuclidianSpace
  val info = new InfoModel

  def add(concept: Concept): Option[Concept] = {
    space.add(concept)
    label(concept)
    info.add(previous, concept)
    chunk(concept)
  }

  var segments: Vector[Vector[Concept]] = Vector.empty
  var ongoing: Vector[Concept] = Vector.empty
  var previous: Concept = Concept.empty // placeholder

  def label(concept: Concept): Unit = {
    val r = space.norm(concept) * 1.0
    concept.category = space.concepts
      .sortBy(concept => info.info(concept.category))
      .find(space.distance(_, concept) < r)
      .get.category
  }

  def chunk(concept: Concept): Option[Concept] = {
    var category: Option[Concept] = None
    if (info.entropy(previous) < info.entropy(concept)) {
      segments = segments :+ ongoing
//      category = Some(space.transform(Trajectory(interpolate(ongoing))))
      category = Some(space.transform(Trajectory(ongoing)))
      ongoing = Vector.empty
    }
    ongoing = ongoing :+ concept
    previous = concept
    category
  }

  val resolution = 256
  def interpolate(cs: Vector[Concept]): Vector[Concept] = {
    val K = resolution / cs.length // assume resolution > 2 * cs.length
    (cs :+ cs.head).sliding(2)
      .flatMap({ case Vector(Concept(a), Concept(b)) =>
        val delta = (a - b) / (K - 1)
        (0 until K).map(k => Concept(a + (delta * k)))
      }).toVector
  }

  /*
  Linear Interpolation:
  - Each part is of equal length
  - Sum of length of the parts equals the length of the whole
  - Each part (and whole) has the same angle with respect to any given vector
   */

  // Numerical Interpolation: same regardless of space
  //   === Euclidian Linear Interpolation
}
