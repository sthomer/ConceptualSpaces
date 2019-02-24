package space

import org.apache.commons.math3.complex
import org.apache.commons.math3.transform._
import Implicits._

trait Space {
  //  val shape: Vector[Int]
  // shape.length = rank of tensor
  // shape(i) = number dimensions in rank i

  var concepts: Vector[Concept]
  var trajectories: Vector[Trajectory]
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
  def innerProduct(u: Vector[Complex], v: Vector[Complex]): Complex

  def norm(u: Vector[Complex]): Complex = Complex.sqrt(innerProduct(u, u))
}

trait Transform extends Space with InnerProduct {
  def transform(t: Trajectory): Concept

  def inverse(c: Concept): Trajectory
}

trait Categorization extends Space with InnerProduct {
  def categorize(c: Concept): Concept
}

sealed trait TensorLike extends Immutable {

  def length: Int

  def +(that: TensorLike): TensorLike

  def -(that: TensorLike): TensorLike

  def *(that: Complex): TensorLike

  def head: TensorLike = this

  def pad: TensorLike = this

  def padTo(max: Int): TensorLike = this
}

case class Complex(re: Double = 0, im: Double = 0)
  extends complex.Complex(re, im) with TensorLike {

  override def toString: String = Complex.f.format(this)

  def i = Complex(0, this.getReal) // allows for e.g. 1 + 2.i

  val length: Int = 1 // or 0?

  def +(that: TensorLike): TensorLike = that match {
    case that: Complex => this add that
  }

  def -(that: TensorLike): TensorLike = that match {
    case that: Complex => this subtract that
  }

  def *(that: Complex): Complex = this multiply that

  def /(that: Complex): Complex = this divide that
}

case class Tensor(ts: Vector[TensorLike] = Vector()) extends TensorLike {

  def apply(is: Int*): TensorLike = (ts(is.head), is.tail) match {
    case (c: Complex, _) => c
    case (t: Tensor, Nil) => t
    case (t: Tensor, rest) => t(rest: _*)
  }

  def length: Int = ts.length // make val?

  override def head: TensorLike = ts.head

  def ++(that: Tensor): Tensor = Tensor(this.ts ++ that.ts)

  def congruent(that: Tensor): Boolean = (ts.head, that.ts.head) match {
    case (a: Complex, b: Complex) => true
    case (a: Tensor, b: Tensor) => a.ts.length == b.ts.length && a.congruent(b)
    case _ => false
  }

  override def pad: Tensor = {
    def limit(x: Int): Int =
      if (length <= x) x else limit(2 * x)

    padTo(limit(1))
  }

  override def padTo(max: Int): Tensor =
    this ++ Tensor.ofShape(max - length :: shape.tail: _*)

  def shape: List[Int] = {
    def shape(t: Tensor): List[Int] = t.head match {
      case tensor: Tensor => t.length :: shape(tensor)
      case _ => List(t.length)
    }

    shape(this)
  }

  def evens: Tensor =
    Tensor(ts.grouped(2).map(t => t.head).toVector)

  def odds: Tensor =
    Tensor(ts.grouped(2).map(t => t.last).toVector)

  def +(that: TensorLike): TensorLike = that match {
    case that: Tensor => Tensor(ts.zip(that.ts).map({ case (a, b) => a + b }))
  }

  def -(that: TensorLike): TensorLike = that match {
    case that: Tensor => Tensor(ts.zip(that.ts).map({ case (a, b) => a + b }))
  }

  def *(complex: Complex): Tensor =
    Tensor(ts.map(t => t * complex))
}

object Complex {
  val f = new complex.ComplexFormat

  val empty = Complex()

  def sqrt(c: Complex): Complex = c.pow(0.5)

  def exp(c: Complex): Complex = Math.E.pow(c)
}

object Tensor {
  def ofShape(shape: Int*): Tensor = {
    def make(shape: Seq[Int]): TensorLike = shape.toList match {
      case Nil => Complex.empty
      case x :: xs => Tensor(Vector.tabulate(x)(_ => make(xs)))
    }

    make(shape).asInstanceOf[Tensor]
  }

  def from(a: Array[Double]) = Tensor(a.toVector.map(Complex(_, 0)))

}

object Implicits {
  implicit def toComplex(x: complex.Complex): Complex =
    Complex(x.getReal, x.getImaginary)

  implicit def toComplex(x: Complex): complex.Complex =
    new complex.Complex(x.getReal, x.getImaginary)

  implicit def toComplex(x: Double): Complex = Complex(x, 0)

  implicit def toComplex(x: Int): Complex = Complex(x, 0)
}

class EuclidianSpace extends Space with Euclidian with FourierTransform {
  var concepts: Vector[Concept] = Vector.empty
  var trajectories: Vector[Trajectory] = Vector.empty
}

// TODO: Manhattan distance InnerProduct/Norm

// Alternatively, Frobenius Inner Product?
trait Euclidian extends InnerProduct {
  def innerProduct(u: Vector[Complex], v: Vector[Complex]): Complex =
    Complex()

  //    (u zip v).map(p => p._1 * p._2).reduce(_ + _)
}

trait FourierTransform extends Transform {

  private def apacheFFT =
    new FastFourierTransformer(DftNormalization.STANDARD)
      .transform(_: Array[complex.Complex], TransformType.FORWARD)

  private def apacheIFFT =
    new FastFourierTransformer(DftNormalization.STANDARD)
      .transform(_: Array[complex.Complex], TransformType.INVERSE)

  // only works on 1-D
  def apacheTransform(trajectory: Trajectory): Concept = {
    val a = apacheFFT(trajectory.tensor.pad
      .ts.map({ case c: complex.Complex => c }).toArray)
    Concept(Tensor(a.map(c => Complex(c.getReal, c.getImaginary)).toVector))
  }

  // only works on 1-D
  def apacheInverse(concept: Concept): Trajectory = concept.tensor match {
    case tensor: Tensor =>
      val a = apacheIFFT(tensor.ts.map({ case c: complex.Complex => c }).toArray)
      Trajectory(a.map(c => Concept(Complex(c.getReal, c.getImaginary))).toVector)
  }

  def transform(trajectory: Trajectory): Concept =
    Concept(fft(trajectory.tensor.pad, Direction.Forward))

  def inverse(concept: Concept): Trajectory = concept.tensor match {
    case tensor: Tensor => fft(tensor, Direction.Inverse) match {
      case tensor: Tensor => Trajectory(tensor.ts.map(c => Concept(c)))
    }
  }


  def fft(t: TensorLike, dir: Direction.Direction): TensorLike = {
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

    def omega(n: Int, m: Int): Complex = dir match {
      case _: Forward => Complex.exp(-2 * Math.PI * m.i / n)
      case _: Inverse => Complex.exp(2 * Math.PI * m.i / n)
    }

    _fft(t) * (dir match {
      case _: Forward => 1.0
      case _: Inverse => 1.0 / t.length
    })
  }
}

object Direction {

  sealed trait Direction

  trait Forward extends Direction

  trait Inverse extends Direction

  object Forward extends Forward

  object Inverse extends Inverse

}
