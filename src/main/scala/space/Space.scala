package space

// import org.apache.commons.math3.transform._
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

trait MixedRadius extends Categorization {
  // When a new concept is added, immediately categorize
  //   with categories instead of concepts
  // Each category has its own mean and variance => mixed radius
}

trait CommonRadius extends Categorization {
  // 1) Within radius of another category
  // 2) Decrease in mean information content
  //    -> post-transform, this is nearly always the case
  //    => don't worry about it...

  // Potentially useful data structures:
  // Map from target to cluster
  // Map from cluster to center
  // Map from target to center??

  var clumps: Vector[Set[Concept]] = Vector()
  var M: Double = 0 // placeholder
  var S: Double = 0 // placeholder
  def V: Double = S / (concepts.length - 1)

  def sd: Double = math.sqrt(V)

  def seed(concept: Concept): Unit = {
    concepts = concepts :+ concept
    clumps = clumps :+ Set[Concept](concept)
    M = math.log(norm(concept) + 1)
    S = 0
  }

  /*
  Mean and variance are taken from the norms of concepts.
  This is equivalent to lining up all concept vectors, and find the stddev.
  The clumping radius is one stddev, so nearby vectors will also be clumped,
  even if they are off-parallel.
   */
  def clump(target: Concept): Set[Concept] = {
    clumps.filter(cl => cl.exists(c => {
      val radius = sd // i.e. 0.5:38%, 0.68:50%, 1:68%, 2: 95%
      val targetDistance = math.log(distance(c, target) + 1)
      val d = targetDistance - radius
      d < 0
    })).flatten.toSet + target
  }

  // Running Mean and Variance
  // The Art of Programming - Knuth Vol. 2 pg. 232 3rd ed.
  def feed(concept: Concept): Unit = {
    concepts = concepts :+ concept
    if (concept.tensor finite) {
      val x = math.log(norm(concept) + 1)
      val Mk = M + ((x - M) / concepts.length)
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
  def distance(a: Concept, b: Concept): Double = {
    val diff = a.tensor - b.tensor
    val c = Complex.sqrt((Tensor.conjugate(diff) * diff).sum)
    c.magnitude
  }

  def norm(c: Concept): Double =
    Complex.sqrt((Tensor.conjugate(c.tensor) * c.tensor).sum).magnitude
}

//trait Manhattan extends InnerProduct {
//  def distance(a: Concept, b: Concept): Double = {
//    val diff = a.tensor - b.tensor // this should be abs(...)
//    diff.asInstanceOf[Tensor].sum.magnitude
//  }
//}

class EuclidianSpace extends Space
  with Euclidian with FourierTransform with CommonRadius {
  var concepts: Vector[Concept] = Vector.empty
  var trajectories: Vector[Trajectory] = Vector.empty
}

class FastEuclidianSpace extends Space
  with Euclidian with FastFourierTransform with CommonRadius {
  var concepts: Vector[Concept] = Vector.empty
  var trajectories: Vector[Trajectory] = Vector.empty
}

class FastLogEuclidianSpace extends Space
  with Euclidian with FastLogFourierTransform with CommonRadius {
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

trait FourierTransform extends Transform {

  def transform(trajectory: Trajectory): Concept =
    Concept(dft(trajectory.tensor, Direction.Forward))

  def inverse(concept: Concept): Trajectory = concept.tensor match {
    case tensor: Tensor => dft(tensor, Direction.Inverse) match {
      case tensor: Tensor => Trajectory(tensor.ts.map(c => Concept(c)))
    }
  }

  def dft(t: Tensor, dir: Direction.Direction): Tensor = {
    import Direction._
    val N = t.length

    def omega(n: Int, k: Int): Complex = (dir match {
      case _: Forward => Complex.exp(-2 * Math.PI * k * n.i / N)
      case _: Inverse => Complex.exp(2 * Math.PI * k * n.i / N)
    }).asInstanceOf[Complex]

    val fns = (0 until N) map (n => t.ts(n) * omega(n, _: Int))
    val ts = (0 until N) map (k => fns map (fn => fn(k)) reduce (_ + _))

    (Tensor(ts toVector) * (dir match {
      case _: Forward => 1.0
      case _: Inverse => 1.0 / N
    })).asInstanceOf[Tensor]
  }
}

trait FastFourierTransform extends Transform {

  def transform(trajectory: Trajectory): Concept =
    Concept(fft(trajectory.tensor.pad, Direction.Forward))

  def inverse(concept: Concept): Trajectory = concept.tensor match {
    case tensor: Tensor => fft(tensor, Direction.Inverse) match {
      case tensor: Tensor => Trajectory(tensor.ts.map(c => Concept(c)))
    }
  }

  def fft(t: Tensor, dir: Direction.Direction): TensorLike = {
    import Direction._

    def _fft(t: Tensor): Tensor = t match {
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
      case _: Forward => Complex.exp(-2 * Math.PI * m.i / n)
      case _: Inverse => Complex.exp(2 * Math.PI * m.i / n)
    }).asInstanceOf[Complex]

    val done = _fft(t)
    dir match {
      case _: Forward => done
      case _: Inverse => (1.0 / t.length) * done
    }
  }
}

trait FastLogFourierTransform extends Transform {

  def transform(trajectory: Trajectory): Concept =
    Concept(fft(trajectory.tensor.pad, Direction.Forward))

  def inverse(concept: Concept): Trajectory = concept.tensor match {
    case tensor: Tensor => fft(tensor, Direction.Inverse) match {
      case tensor: Tensor => Trajectory(tensor.ts.map(c => Concept(c)))
    }
  }

  def fft(t: Tensor, dir: Direction.Direction): TensorLike = {
    import Direction._

    def _fft(t: Tensor): Tensor = t match {
      case base if t.length == 1 => // base
        // Natural log transform
        dir match {
          case _: Forward => base
          case _: Inverse => base match {
            case Tensor(Vector(c: Complex)) => Tensor(Vector(c.exp))
            case Tensor(Vector(t: Tensor)) => Tensor(Vector(t.exp))
          }
        }
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
      case _: Forward => Complex.exp(-2 * Math.PI * m.i / n)
      case _: Inverse => Complex.exp(2 * Math.PI * m.i / n)
    }).asInstanceOf[Complex]

    val done = _fft(t)
    dir match {
      case _: Forward => done.log
      case _: Inverse => (1.0 / t.length) * done
    }
  }
}

trait Kmeans extends Categorization {

  def min: Concept = // lazy val?
    Concept(concepts.map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => Tensor.min(acc, t)))

  def max: Concept = // lazy val?
    Concept(concepts.map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => Tensor.max(acc, t)))

  def mean: Concept =
    Concept(concepts.map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => acc + t) / concepts.length)

  def stddev(mean: Concept): Concept =
    Concept(Tensor.sqrt(concepts.map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => {
      val diff = t - mean.tensor
      acc + (diff * diff)
    }) / concepts.length))

  def random(min: Concept, max: Concept): Concept = // lazy val?
    Concept(Tensor.random(min.tensor, max.tensor))

  def center(members: Vector[Concept]): Concept =
    Concept(members
      .map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => acc + t)
      / members.length)

  def partition(centers: Vector[Concept]): Map[Concept, Vector[Concept]] =
    concepts
      .map(c => centers.reduce((acc, p) =>
        if (distance(c, acc) < distance(c, p)) acc else p) -> c)
      .groupBy(_._1).mapValues(_.map(_._2)) // convert p->c pairs to a map

  def move(partition: Map[Concept, Vector[Concept]]): Vector[Concept] =
    partition.values.map(ms => center(ms)).toVector

  def quantize: Vector[Concept] = {
    var count = 1
    //    val allMinC = min
    //    val allMaxC = max
    val meanC = mean
    val stddevC = stddev(mean)
    val minC = Concept(Tensor.sqrt(meanC.tensor - stddevC.tensor))
    val maxC = Concept(Tensor.sqrt(meanC.tensor + stddevC.tensor))
    val initial = concepts.map(_ => random(minC, maxC))

    def update(centers: Vector[Concept]): Map[Concept, Vector[Concept]] = {
      count = count + 1
      val p = partition(centers)
      val c = move(p)
      if (count == 10 || c == centers) p else update(c) // 10 = sweet spot
    }

    val map = update(initial).flatMap({ case (c, ms) => ms.map(m => m -> c) })
    concepts.map(c => map(c))
  }
}

//var clumps: Vector[Set[Concept]] = Vector()
//  var M: TensorLike = 0 // placeholder
//  var S: TensorLike = 0 // placeholder
//  def V: TensorLike = S / (concepts.length - 1)
//
//  def sd: TensorLike = Tensor.sqrt(V)
//
//  def seed(concept: Concept): Unit = {
//    concepts = concepts :+ concept
//    clumps = clumps :+ Set[Concept](concept)
//    M = concept.tensor
//    S = 0
//  }
//
//  def clump(target: Concept): Set[Concept] = {
//    val x = target.tensor
//    if (x finite) { // denominator is slightly too big
//      val Mk = M + ((x - M) / concepts.length)
//      val Sk = S + ((x - M) * (x - Mk))
//      M = Mk
//      S = Sk
//    }
//
//    clumps.filter(cl => cl.exists(c => {
//      val s = sd * 2
//      val v = norm(Concept(V))
//      val targetDistance = distance(c, target)
//      val radiusDistance = distance(c, Concept(c.tensor + sd))
//      val d = targetDistance - radiusDistance
//      d < 0
//    })).flatten.toSet + target
//  }
//
//  def feed(concept: Concept): Unit = {
//      concepts = concepts :+ concept
//
//      val cl = clump(concept)
//      clumps = (clumps :+ cl).map(clump =>
//        if ((clump & cl) != Set.empty) clump | cl else clump).distinct
//    }

//object Trajectory {
//  def fromConcepts(concepts: Vector[Concept]): Trajectory = {
//    val max = (0 /: concepts.map(_.tensor.length)) (math.max)
//    Trajectory(concepts.map(c => Concept(c.tensor.padTo(max))))
//  }
//}

//trait ApacheFourierTransform extends Transform {
//
//  private def apacheFFT =
//    new FastFourierTransformer(DftNormalization.STANDARD)
//      .transform(_: Array[complex.Complex], TransformType.FORWARD)
//
//  private def apacheIFFT =
//    new FastFourierTransformer(DftNormalization.STANDARD)
//      .transform(_: Array[complex.Complex], TransformType.INVERSE)
//
//  // only works on 1-D
//  def apacheTransform(trajectory: Trajectory): Concept = {
//    val a = apacheFFT(trajectory.tensor.pad
//      .ts.map({ case c: complex.Complex => c }).toArray)
//    Concept(Tensor(a.map(c => Complex(c.getReal, c.getImaginary)).toVector))
//  }
//
//  // only works on 1-D
//  def apacheInverse(concept: Concept): Trajectory = concept.tensor match {
//    case tensor: Tensor =>
//      val a = apacheIFFT(tensor.ts.map({ case c: complex.Complex => c }).toArray)
//      Trajectory(a.map(c => Concept(Complex(c.getReal, c.getImaginary))).toVector)
//  }
//}

//  // => This is useless
//  // Mean is very close to 0 and Variance is negligible
//  // Only works for time-domain signal
//  def quantize(): Unit = {
//  // Quantize concepts into N bins between min and max of Concept values
//  // => no need to merge b/c values are rounded/truncated to a bin
//  // Use quantizing method from Lexicon Formation?
//  //   i.e. mean +- 2*stddev w/ clipping
//  //   Assume normal dist -> break into bins of X% from mean
//  //     sigma * (1 / sqrt(1 - x) = x% of population
//
//    import scala.math._
//    val N = 100
//    val amplitudes = concepts.map(_.tensor).map({ case c: Complex => c.getReal })
//    val mean = amplitudes.sum / amplitudes.length
//    val variance = amplitudes.map(a => a - mean).sum / amplitudes.length
//    val sd = sqrt(amplitudes.map(a => a - mean).sum / amplitudes.length)
//    // could use gaussian cumulative distribution for equal prob bins
//    val bin = 4 * sd / N // all bins equal
//    val lower = mean - 2 * sd
//    val upper = mean + 2 * sd
//
//    def quantize(value: Double, level: Double): Double = {
//      if (max(value, level) != level) level
//      else quantize(value, level + bin)
//    }
//
//    val quantized = amplitudes.map(amp => amp match {
//      case a if a <= lower => lower
//      case a if a >= upper => upper
//      case a => quantize(a, lower)
//    })
//
//    concepts = quantized.map(r => Concept(r))
//  }

//  val fuzz = 0.00005 //  fuzz radius hyperparameter
//  def merge(a: Concept, b: Concept): Option[Concept] = {
//    val d = distance(a, b)
//    if (d <= fuzz)
//      Some(Concept((a.tensor + b.tensor) / 2))
//    else None
//  }
//
//  // Far too slow
//  def fuzzify(): Unit = {
//    val arr = concepts.toArray
//    for {
//      a <- concepts.indices
//      b <- a + 1 until concepts.length
//    } yield merge(concepts(a), concepts(b)) match {
//      case None => Unit
//      case Some(c) =>
//        arr(a) = c
//        arr(b) = c
//    }
//    concepts = arr.toVector
//  }

//  def seed(concept: Concept): Unit = {
//    concepts = concepts :+ concept
//    clumps = clumps :+ Set[Concept](concept)
//    M = concept.tensor
//    S = 0
//  }
//
//  def clump(target: Concept): Set[Concept] = {
//    val x = target.tensor
//    if (x finite) { // denominator is slightly too big
//      val Mk = M + ((x - M) / concepts.length)
//      val Sk = S + ((x - M) * (x - Mk))
//      M = Mk
//      S = Sk
//    }
//
//    clumps.filter(cl => cl.exists(c => {
//      val s = sd
//      val v = norm(Concept(V))
//      val targetDistance = distance(c, target)
//      val radiusDistance = distance(c, Concept(c.tensor + sd))
//      val d = targetDistance - radiusDistance
//      d < 0
//    })).flatten.toSet + target
//  }

