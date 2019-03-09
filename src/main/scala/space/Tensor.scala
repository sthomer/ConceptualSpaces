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
    case _ => c.pow(0.5) //
  }

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


