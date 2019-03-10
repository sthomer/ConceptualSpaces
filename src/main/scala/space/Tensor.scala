package space

import org.apache.commons.math3.complex
import Implicits._

object Implicits {
  implicit def toComplex(x: complex.Complex): Complex =
    Complex(x.getReal, x.getImaginary)

  implicit def toComplex(x: Complex): complex.Complex =
    new complex.Complex(x.getReal, x.getImaginary)

  implicit def toComplex(x: Double): Complex = Complex(x, 0)

  implicit def toComplex(x: Int): Complex = Complex(x, 0)
}

object Complex {
  val f = new complex.ComplexFormat

  val empty = Complex()

  def sqrt(c: Complex): Complex = c match {
    case Complex.empty => c
    case _ => c.pow(0.5)
  }

  def conjugate(c: Complex): Complex = c match {
    case Complex.empty => c
    case _ => c.pow(0.5)
  }

  def exp(c: Complex): Complex = Math.E.pow(c)

  def exp(that: TensorLike): TensorLike = that match {
    case c: Complex => Math.E.pow(c)
    case t: Tensor => Tensor(t.ts.map(exp))
  }
}

object Tensor {
  val r = new scala.util.Random // nextDouble or nextGaussian?

  def ofShape(shape: Int*): Tensor = {
    def make(shape: Seq[Int]): TensorLike = shape.toList match {
      case Nil => Complex.empty
      case x :: xs => Tensor(Vector.tabulate(x)(_ => make(xs)))
    }

    make(shape).asInstanceOf[Tensor]
  }

  def sqrt(t: TensorLike): TensorLike = t match {
    case c: Complex => c.sqrt
    case t: Tensor => Tensor(t.ts.map(sqrt))
  }

  def conjugate(t: TensorLike): TensorLike = t match {
    case c: Complex => c.conjugate
    case t: Tensor => Tensor(t.ts.map(conjugate))
  }

  def random(min: TensorLike, max: TensorLike): TensorLike = {
    def make(min: TensorLike, max: TensorLike): TensorLike = (min, max) match {
      case (min: Complex, max: Complex) => Complex(
        r.nextDouble * (max.re - min.re) + min.re, // or (r.nextGaussian + 0.5)
        r.nextDouble * (max.im - min.im) + min.im) // or (r.nextGaussian + 0.5)
      case (min: Tensor, max: Tensor) =>
        Tensor(min.ts.zip(max.ts).map({ case (min, max) => make(min, max) }))
    }
    make(min, max)
  }


  def from(a: Array[Double]) = Tensor(a.toVector.map(Complex(_, 0)))

  def min(a: TensorLike, b: TensorLike): TensorLike = {
    def minRe(a: TensorLike, b: TensorLike): TensorLike = (a, b) match {
      case (a: Complex, b: Complex) =>
        Complex(math.min(a.re, b.re), 0)
      case (a: Tensor, b: Tensor) =>
        Tensor(a.ts.zip(b.ts).map({ case (a, b) => minRe(a, b) }))
    }

    def minIm(a: TensorLike, b: TensorLike): TensorLike = (a, b) match {
      case (a: Complex, b: Complex) =>
        Complex(0, math.max(a.im, b.im))
      case (a: Tensor, b: Tensor) =>
        Tensor(a.ts.zip(b.ts).map({ case (a, b) => minIm(a, b) }))
    }

    minRe(a, b) + minIm(a, b)
  }

  def max(a: TensorLike, b: TensorLike): TensorLike = {
    def maxRe(a: TensorLike, b: TensorLike): TensorLike = (a, b) match {
      case (a: Complex, b: Complex) =>
        Complex(math.max(a.re, b.re), 0)
      case (a: Tensor, b: Tensor) =>
        Tensor(a.ts.zip(b.ts).map({ case (a, b) => maxRe(a, b) }))
    }

    def maxIm(a: TensorLike, b: TensorLike): TensorLike = (a, b) match {
      case (a: Complex, b: Complex) =>
        Complex(0, math.max(a.im, b.im))
      case (a: Tensor, b: Tensor) =>
        Tensor(a.ts.zip(b.ts).map({ case (a, b) => maxIm(a, b) }))
    }

    maxRe(a, b) + maxIm(a, b)
  }
}

sealed trait TensorLike extends Immutable {

  def length: Int

  def sum: Complex

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

  def sum = this

  val magnitude =
    Math.sqrt(Math.pow(Math.abs(re), 2) + Math.pow(Math.abs(im), 2))

  // Scala Operations

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

  def rounded: Tensor = {
    import scala.math.round
    def rounded(tensor: Tensor): TensorLike =
      Tensor(tensor.ts.map({
        case c: Complex => Complex(round(c.re).toDouble, round(c.im).toDouble)
        case t: Tensor => rounded(t)
      }))

    rounded(this).asInstanceOf[Tensor]
  }

  // This is akin to flattening the tensor to a vector and summing
  def sum: Complex = {
    def sum(tensor: Tensor): TensorLike = {
      val ts = tensor.ts
      if (ts.length == 1) // reduce returns with uneval'ed element if only one
        ts.head match {
          case c: Complex => c
          case t: Tensor => sum(t)
        }
      else
        ts reduce ((acc, t) => acc + t match {
          case c: Complex => c
          case t: Tensor => sum(t)
        })
    }

    val s = sum(this)
    s.asInstanceOf[Complex]
  }

  def evens: Tensor =
    Tensor(ts.grouped(2).map(t => t.head).toVector)

  def odds: Tensor =
    Tensor(ts.grouped(2).map(t => t.last).toVector)

  // Abstract these to a map (maybe also make a reduce?)

  def sqrt: TensorLike = {
    def sqrt(t: TensorLike): TensorLike = t match {
      case c: Complex => c.sqrt
      case t: Tensor => Tensor(t.ts.map(sqrt))
    }
    sqrt(this)
  }

  def conjugate: TensorLike = {
    def conjugate(t: TensorLike): TensorLike = t match {
      case c: Complex => c.conjugate
      case t: Tensor => Tensor(t.ts.map(conjugate))
    }
    conjugate(this)
  }

  // Frobenius Operations

  def +(that: TensorLike): TensorLike = that match {
    case c: Complex => Tensor(ts.map(t => t + c))
    case t: Tensor => Tensor(ts.zip(t.ts).map({ case (a, b) => a + b }))
  }

  def -(that: TensorLike): TensorLike = that match {
    case c: Complex => Tensor(ts.map(t => t - c))
    case t: Tensor => Tensor(ts.zip(t.ts).map({ case (a, b) => a - b }))
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


