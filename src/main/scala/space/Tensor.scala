package space

import org.apache.commons.math3.complex
import Implicits._

object Implicits {

  implicit def toComplex(x: complex.Complex): Complex =
    Complex(x.getReal, x.getImaginary)

  implicit def toComplex(x: Complex): complex.Complex =
    new complex.Complex(x.getReal, x.getImaginary)

  implicit def toComplex(x: Double): Complex =
    Complex(x, 0)

  implicit def toComplex(x: Int): Complex =
    Complex(x, 0)
}

object Complex {

  val f = new complex.ComplexFormat

  val i = new Complex(0, 1)
}

object Tensor {

  def ofShape(shape: Int*): Tensor = {
    def make(shape: Seq[Int]): TensorLike = shape.toList match {
      case Nil => Complex()
      case x :: xs => Tensor(Vector.tabulate(x)(_ => make(xs)))
    }

    make(shape).asInstanceOf[Tensor]
  }
}

sealed trait TensorLike extends Immutable {

  def length: Int

  def sum: Complex

  def finite: Boolean

  def +(that: TensorLike): TensorLike

  def -(that: TensorLike): TensorLike

  def *(that: TensorLike): TensorLike

  def /(that: TensorLike): TensorLike

  def sqrt: TensorLike

  def log: TensorLike

  def exp: TensorLike

  def conjugate: TensorLike

  def pad: TensorLike = this

  def padTo(max: Int): TensorLike = this
}

case class Complex(re: Double = 0, im: Double = 0)
  extends complex.Complex(re, im) with TensorLike {

  override def toString: String = Complex.f.format(this)

  val length: Int = 1

  val sum: Complex = this

  val magnitude: Double = abs

  val finite: Boolean = !(this.isNaN || this.isInfinite)

  override def conjugate: Complex = super.conjugate

  override def exp: Complex = super.exp

  override def log: Complex = super.log

  override def sqrt: Complex = super.sqrt

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

  val length: Int = ts.length // i.e. length of "top-level"

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
    def shape(t: Tensor): List[Int] = t.ts.head match {
      case tensor: Tensor => t.length :: shape(tensor)
      case _ => List(t.length)
    }

    shape(this)
  }

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

  lazy val evens: Tensor =
    Tensor(ts.grouped(2).map(t => t.head).toVector)

  lazy val odds: Tensor =
    Tensor(ts.grouped(2).map(t => t.last).toVector)

  def finite: Boolean = {
    def finite(t: TensorLike): Boolean = t match {
      case c: Complex => c.finite
      case t: Tensor => t.ts.forall(finite)
    }

    finite(this)
  }

  def map(fn: Complex => Complex): TensorLike = {
    def map(t: TensorLike): TensorLike = t match {
      case c: Complex => fn(c)
      case t: Tensor => Tensor(t.ts.map(map))
    }

    map(this)
  }

  def sqrt: TensorLike = map(_.sqrt)

  def conjugate: TensorLike = map(_.conjugate)

  def log: TensorLike = map(_.log)

  def exp: TensorLike = map(_.exp)

  def binOp(that: TensorLike)
           (op: (TensorLike, TensorLike) => TensorLike): TensorLike =
    that match {
      case c: Complex => Tensor(ts.map(t => op(t, c)))
      case t: Tensor => Tensor(ts.zip(t.ts).map({ case (a, b) => op(a, b) }))
    }

  def +(that: TensorLike): TensorLike = binOp(that)(_ + _)

  def -(that: TensorLike): TensorLike = binOp(that)(_ - _)

  def *(that: TensorLike): TensorLike = binOp(that)(_ * _)

  def /(that: TensorLike): TensorLike = binOp(that)(_ / _)
}
