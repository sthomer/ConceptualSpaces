package space

// import org.apache.commons.math3.transform._
import Implicits._

trait Space {
  //  val shape: Vector[Int]
  // shape.length = rank of tensor
  // shape(i) = number dimensions in rank i

  var concepts: Vector[Concept]
  var trajectories: Vector[Trajectory]

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
}

case class Concept(tensor: TensorLike) extends Immutable

case class Trajectory(concepts: Vector[Concept] = Vector()) extends Immutable {
  lazy val tensor = Tensor(concepts map (_.tensor))
}

object Trajectory {
  def fromConcepts(concepts: Vector[Concept]): Trajectory = {
    val max = (0 /: concepts.map(_.tensor.length)) (Math.max(_, _))
    Trajectory(concepts.map(c => Concept(c.tensor.padTo(max))))
  }
}

// This should operate on TensorLike and return a TensorLike
// b/c higher-rank inner products (n, m) -> n + m - 2
trait InnerProduct extends Space {
  def distance(a: Concept, b: Concept): Double

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
}

trait Transform extends Space with InnerProduct {
  def transform(t: Trajectory): Concept

  def inverse(c: Concept): Trajectory
}

trait Categorization extends Space with InnerProduct

class EuclidianSpace extends Space with Euclidian with FourierTransform {
  var concepts: Vector[Concept] = Vector.empty
  var trajectories: Vector[Trajectory] = Vector.empty
}

class FastEuclidianSpace extends Space with Euclidian with FastFourierTransform {
  var concepts: Vector[Concept] = Vector.empty
  var trajectories: Vector[Trajectory] = Vector.empty
}

trait Euclidian extends InnerProduct {
  def distance(a: Concept, b: Concept): Double = {
    val diff = a.tensor - b.tensor
    Complex.sqrt((diff * diff).sum).magnitude
  }
}

trait Manhattan extends InnerProduct {
  def distance(a: Concept, b: Concept): Double = {
    val diff = a.tensor - b.tensor // this should be abs(...)
    diff.asInstanceOf[Tensor].sum.magnitude
  }
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

object Direction {

  sealed trait Direction

  trait Forward extends Direction

  trait Inverse extends Direction

  object Forward extends Forward

  object Inverse extends Inverse

}

trait FastFourierTransform extends Transform {

  def transform(trajectory: Trajectory): Concept =
    Concept(fft(trajectory.tensor.pad, Direction.Forward)/*.rounded*/)

  def inverse(concept: Concept): Trajectory = concept.tensor match {
    case tensor: Tensor => fft(tensor, Direction.Inverse)/*.rounded*/ match {
      case tensor: Tensor => Trajectory(tensor.ts.map(c => Concept(c)))
    }
  }

  def fft(t: TensorLike, dir: Direction.Direction): Tensor = {
    import Direction._

    def _fft(t: TensorLike): TensorLike = t match {
      case c if t.length == 1 => c
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

    (_fft(t) * (dir match {
      case _: Forward => 1.0
      case _: Inverse => 1.0 / t.length
    })).asInstanceOf[Tensor]
  }
}

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
