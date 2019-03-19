package space

import Implicits._

trait Space { // make immutable??
  var concepts: Vector[Concept]
  var trajectories: Vector[Trajectory]
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

trait Categorization extends Space with InnerProduct {
  //  def quantize: Vector[Concept]
}

trait CommonRadius extends Categorization {
  // 1) Within radius of another category
  // 2) Decrease in mean information content
  //    -> post-transform, this is nearly always the case
  //    => don't worry about it...
  val silenceUnion: Boolean = false

  var clumps: Vector[Set[Concept]] = Vector()
  var distincts: Set[Concept] = Set.empty
  var min: Concept = Concept(0) //placeholder
  var M: TensorLike = 0 // placeholder
  var S: TensorLike = 0 // placeholder

  def sd: TensorLike =
    if (distincts.size == 1) S
    else (S / (distincts.size - 1)).sqrt

  def seed(concept: Concept): Unit = {
    concepts = concepts :+ concept
    clumps = clumps :+ Set[Concept](concept)
    distincts = distincts + concept
    M = concept.tensor
    S = 0
  }

  // How to avoid union with silence?
  // Explicitly forbid clumping with minimum concept
  //   Minimum concept represents silence
  //   Allows for larger clumping radius
  // Allow silence clumping at first level, forbid thereafter
  def clump(target: Concept): Set[Concept] = {
    val s = norm(Concept(sd))
    val radius = s * 0.68 // (if (silenceUnion) 0.68 else 2) // 50% vs 95%
    if (silenceUnion || (norm(target) > 0.05 * radius)) {
      clumps.filter(cl => cl.exists(c => {
        val targetDistance = distance(c, target)
        val d = targetDistance - radius
        (silenceUnion || (norm(c) > 0.05 * radius)) &&
          d < 0
      })).flatten.toSet + target
    } else Set(target)
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

  def fill(concepts: Vector[Concept]): Unit = {
    seed(concepts.head)
    concepts.tail foreach feed
  }

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

trait Euclidian extends InnerProduct {
  def norm(c: Concept): Double =
    (c.tensor.conjugate * c.tensor).sqrt.sum.magnitude

  def distance(a: Concept, b: Concept): Double =
    norm(Concept(a.tensor - b.tensor))

  def normalize(c: Concept): Concept =
    Concept(c.tensor / norm(c))
}

class EuclidianSpace extends Space
  with Euclidian with FastFourierTransform with CommonRadius {
  var concepts: Vector[Concept] = Vector.empty
  var trajectories: Vector[Trajectory] = Vector.empty
}

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
