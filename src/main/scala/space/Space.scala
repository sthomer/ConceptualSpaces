package space

import Implicits._

trait Space { // make immutable??
  var concepts: Vector[Concept] = Vector.empty
  var trajectories: Vector[Trajectory] = Vector.empty
}

object Concept {
  def combine(cs: Vector[Concept]): Concept = Concept(cs
    .map(c => c.tensor)
    .reduce[TensorLike]((acc, t) => acc + t)
    / cs.length)
}

case class Concept(tensor: TensorLike) extends Immutable

case class Trajectory(concepts: Vector[Concept] = Vector()) extends Immutable {
  lazy val tensor = Tensor(concepts map (_.tensor))
}

trait InnerProduct extends Space {
  def distance(a: Concept, b: Concept): Double

  def norm(c: Concept): Double
}

trait Transform extends Space with InnerProduct {
  def transform(t: Trajectory): Concept

  def inverse(c: Concept): Trajectory
}

trait Interpolation extends Space with InnerProduct {
  val resolution = 256

  def interpolate(cs: Vector[Concept]): Vector[Concept]
}

trait LinearFill extends Interpolation {

  /*
  Linear Interpolation:
  - Each part is of equal length
  - Sum of length of the parts equals the length of the whole
  - Each part (and whole) has the same angle with respect to any given vector
   */

  def interpolate(cs: Vector[Concept]): Vector[Concept] = {
    val K = resolution / cs.length // assume resolution > cs.length
    (cs :+ cs.head).sliding(2)
      .flatMap({ case Vector(Concept(a), Concept(b)) =>
        val delta = (a - b) / (K - 1) // assumes Euclidian inner product
        (0 until K).map(k => Concept(a + (delta * k)))
      }).toVector
  }
}

trait Segmentation extends Space with InnerProduct

trait RisingEntropy extends Segmentation {
  val m = new Model
  var segments: Vector[Vector[Concept]] = Vector()
  var ongoing: Vector[Concept] = Vector()

  def add(current: Concept, next: Concept): Unit = m.add(current, next)

  def detect(previous: Concept, current: Concept): Boolean = {
    //    val c = m.entropy(current)
    //    println(c)
    m.entropy(previous) < m.entropy(current)
  }

  //  def segment(current: Concept, next: Concept): Concept =
  //    Concept.combine(current, next)

  def chop(concepts: Vector[Concept]): Unit =
    concepts.sliding(3).foreach({
      case Vector(previous: Concept, current: Concept, next: Concept) =>
        add(current, next)
        if (detect(previous, current)) {
          segments = segments :+ ongoing
          ongoing = Vector()
        }
        ongoing = ongoing :+ current
    })

  def segmentize: Vector[Concept] =
    segments.flatMap(segment => {
      val concept = Concept.combine(segment) // Interpolate + Transform
      segment.map(c => concept)
    })
}

trait Categorization extends Space with InnerProduct

trait VarianceRadius extends Categorization {
  var clumps: Vector[Set[Concept]] = Vector()
  var distincts: Set[Concept] = Set.empty
  var M: TensorLike = 0 // placeholder
  var S: TensorLike = 0 // placeholder

  def sd: TensorLike =
    if (distincts.size == 1) S
    else (S / (distincts.size - 1)).sqrt

  def radius: Double = norm(Concept(sd)) * 0.68

  def clump(target: Concept): Set[Concept] = {
    val r = radius
    (if (norm(target) > 0.05 * r)
      clumps.filter(cl => cl.exists(c =>
        norm(c) > 0.05 * r && distance(c, target) < r
      )).flatten.toSet
    else Set[Concept]()) + target
  }

  def feed(concept: Concept): Unit = {
    concepts = concepts :+ concept
    val x = concept.tensor
    if (!distincts(concept) && concept.tensor.finite) {
      distincts = distincts + concept
      val Mk = M + ((x - M) / distincts.size)
      val Sk = S + ((x - M) * (x - Mk))
      M = Mk
      S = Sk
    }

    val cl = clump(concept)
    clumps = (clumps :+ cl).map(clump =>
      if ((clump & cl) != Set.empty) clump | cl else clump).distinct
  }

  def fill(concepts: Vector[Concept]): Unit = concepts foreach feed

  def center(members: Set[Concept]): Concept =
    Concept(members
      .map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => acc + t)
      / members.size)

  def categorize: Vector[Concept] = {
    val centers = clumps.map(cl => cl -> center(cl)).toMap
    concepts.flatMap(c => clumps.find(cl => cl(c)))
      .map(cl => centers(cl))
  }
}

trait NoiseRadius extends VarianceRadius {
  override def clump(target: Concept): Set[Concept] = {
    val r = radius
    clumps.filter(cl => cl.exists(c => distance(c, target) < r))
      .flatten.toSet + target
  }
}

trait Euclidian extends InnerProduct {
  def norm(c: Concept): Double =
    (c.tensor.conjugate * c.tensor).sqrt.sum.magnitude

  def distance(a: Concept, b: Concept): Double =
    norm(Concept(a.tensor - b.tensor))

  def normalize(c: Concept): Concept =
    Concept(c.tensor / norm(c))
}

class RawEuclidianSpace extends Space
  with Euclidian with FastFourierTransform with NoiseRadius

class EuclidianSpace extends Space
  with Euclidian with FastFourierTransform
  with VarianceRadius with RisingEntropy with LinearFill

object Direction {

  sealed trait Direction

  trait Forward extends Direction

  trait Inverse extends Direction

  object Forward extends Forward

  object Inverse extends Inverse

}

trait FastFourierTransform extends Transform {

  def transform(trajectory: Trajectory): Concept = Concept(
    fft(trajectory.tensor.pad, Direction.Forward))

  def inverse(concept: Concept): Trajectory = Trajectory(
    fft(concept.tensor, Direction.Inverse) match {
      case c: Complex => Vector(Concept(c))
      case t: Tensor => t.ts.map(c => Concept(c))
    })

  def fft(t: TensorLike, dir: Direction.Direction): TensorLike = {

    def _fft(t: TensorLike): TensorLike = t match {
      case base if t.length == 1 => base
      case t: Tensor =>
        val n = t.length
        (_fft(t.evens), _fft(t.odds)) match {
          case (evens: Tensor, odds: Tensor) =>
            val c1 = (0 until n / 2) map (m => evens(m) + odds(m) * omega(n, m))
            val c2 = (0 until n / 2) map (m => evens(m) - odds(m) * omega(n, m))
            Tensor((c1 ++ c2).toVector)
        }
    }

    def omega(n: Int, m: Int): Complex = (dir match {
      case _: Direction.Forward => (-2 * math.Pi * Complex.I * m / n).exp
      case _: Direction.Inverse => (2 * math.Pi * Complex.I * m / n).exp
    }).asInstanceOf[Complex]

    val done = _fft(t)
    dir match {
      case _: Direction.Forward => done
      case _: Direction.Inverse => (1.0 / t.length) * done
    }
  }
}
