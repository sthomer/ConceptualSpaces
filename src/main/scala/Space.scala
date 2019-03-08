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
  //  def innerProduct(u: Vector[Complex], v: Vector[Complex]): Complex

  //  def norm(u: Vector[Complex]): Complex = Complex.sqrt(innerProduct(u, u))

  def distance(a: Concept, b: Concept): Complex // or Real??
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

  def *(that: TensorLike): TensorLike

  def /(that: TensorLike): TensorLike

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
    case c: Complex => this add c
    case t: Tensor => t + this
  }

  def -(that: TensorLike): TensorLike = that match {
    case c: Complex => this subtract c
    case t: Tensor => t - this
  }

  def *(that: TensorLike): TensorLike = that match {
    case c: Complex => this multiply c
    case t: Tensor => t * this
  }

  def /(that: TensorLike): TensorLike = that match {
    case c: Complex => this divide c
    case t: Tensor => t / this
  }
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

  // StackOverFLowError
  def sum: Complex = {
    def sum(t: Tensor): TensorLike =
      ts reduce ((acc, t) => acc + t match {
        case c: Complex => c
        case t: Tensor => sum(t)
      })

    sum(this).asInstanceOf[Complex]
  }

  def evens: Tensor =
    Tensor(ts.grouped(2).map(t => t.head).toVector)

  def odds: Tensor =
    Tensor(ts.grouped(2).map(t => t.last).toVector)

  def +(that: TensorLike): TensorLike = that match {
    case c: Complex => Tensor(ts.map(t => t + c))
    case t: Tensor => Tensor(ts.zip(t.ts).map({ case (a, b) => a + b }))
  }

  def -(that: TensorLike): TensorLike = that match {
    case c: Complex => Tensor(ts.map(t => t - c))
    case t: Tensor => Tensor(ts.zip(t.ts).map({ case (a, b) => a + b }))
  }

  def *(that: TensorLike): TensorLike = that match {
    case c: Complex => Tensor(ts.map(t => t * c))
    case t: Tensor => Tensor(ts.zip(t.ts).map({ case (a, b) => a * b }))
  }

  def /(that: TensorLike): TensorLike = that match {
    case c: Complex => Tensor(ts.map(t => t / c))
    case t: Tensor => Tensor(ts.zip(t.ts).map({ case (a, b) => a / b }))
  }
}

object Complex {
  val f = new complex.ComplexFormat

  val empty = Complex()

  def sqrt(c: Complex): Complex = c.pow(0.5)

  def exp(c: Complex): Complex = Math.E.pow(c)

  def exp(that: TensorLike): TensorLike = that match {
    case c: Complex => Math.E.pow(c)
    case t: Tensor => Tensor(t.ts.map(exp))
  }
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

  def distance(a: Concept, b: Concept): Complex = {
    val diff = a.tensor - b.tensor
    Complex.sqrt((diff * diff).asInstanceOf[Tensor].sum)
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
//
//}
//
//trait FastFourierTransform extends Transform {
//
//  def transform(trajectory: Trajectory): Concept =
//    Concept(fft(trajectory.tensor.pad, Direction.Forward))
//
//  def inverse(concept: Concept): Trajectory = concept.tensor match {
//    case tensor: Tensor => fft(tensor, Direction.Inverse) match {
//      case tensor: Tensor => Trajectory(tensor.ts.map(c => Concept(c)))
//    }
//  }
//
//
//  def fft(t: TensorLike, dir: Direction.Direction): TensorLike = {
//    import Direction._
//
//    def _fft(t: TensorLike): TensorLike = t match {
//      case c if t.length == 1 => c
//      case t: Tensor =>
//        val n = t.length
//        (_fft(t.evens), _fft(t.odds)) match {
//          case (evens: Tensor, odds: Tensor) =>
//            val c1 = (0 until n / 2) map (m => evens(m) + odds(m) * omega(n, m))
//            val c2 = (0 until n / 2) map (m => evens(m) - odds(m) * omega(n, m))
//            Tensor((c1 ++ c2).toVector)
//        }
//    }
//
//    def omega(n: Int, m: Int): Complex = (dir match {
//      case _: Forward => Complex.exp(-2 * Math.PI * m.i / n)
//      case _: Inverse => Complex.exp(2 * Math.PI * m.i / n)
//    }).asInstanceOf[Complex]
//
//    _fft(t) * (dir match {
//      case _: Forward => 1.0
//      case _: Inverse => 1.0 / t.length
//    })
//  }
//}
