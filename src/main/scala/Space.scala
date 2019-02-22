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

case class Trajectory(concepts: Array[Concept]) extends Immutable {
  lazy val tensor = Tensor(concepts map (_.tensor))
}

object Trajectory {
  def fromTensor(t: TensorLike): Trajectory = t match {
    case c: Complex => Trajectory(List(Concept(c)).toArray)
    case t: Tensor => Trajectory(t.ts map Concept)
  }

  def fromConcepts(concepts: Array[Concept]): Trajectory = {
    val max = (0 /: concepts.map(_.tensor.toArray.length)) (Math.max(_, _))
    Trajectory(concepts.map(c =>
      Concept(Tensor.from(NdArray.padTo(c.tensor.toArray, max)))))
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


sealed trait TensorLike {
  def toArray: Array[_ <: Any]

  def toRealArray: Array[_ <: Any]
}

case class Tensor(ts: Array[TensorLike]) extends TensorLike {
  def apply(is: Int*): TensorLike = (ts(is.head), is.tail) match {
    case (c: Complex, _) => c
    case (t: Tensor, Nil) => t
    case (t: Tensor, rest) => t(rest: _*)
  }

  def toArray: Array[_ <: Any] = ts map {
    case complex: Complex => complex
    case tensor: Tensor => tensor.toArray
  }

  def toRealArray: Array[_ <: Any] = ts map {
    case complex: Complex => complex.getReal
    case tensor: Tensor => tensor.toRealArray
  }

  def +=(c: Complex, is: List[Int]): Unit = {
    (ts(is.head), is.tail) match {
      case (_: Complex, Nil) => ts(is.head) = c
      case (t: Tensor, rest) => t += (c, rest)
    }
  }

  def congruent(that: Tensor): Boolean = (ts.head, that.ts.head) match {
    case (a: Complex, b: Complex) => true
    case (a: Tensor, b: Tensor) => a.ts.length == b.ts.length && a.congruent(b)
    case _ => false
  }
}

object Tensor {

  def ofShape(shape: Int*): Tensor = {
    def make(shape: Seq[Int]): TensorLike = shape.toList match {
      case Nil => 0.i
      case x :: xs => Tensor(Array.tabulate(x)(_ => make(xs)))
    }

    make(shape).asInstanceOf[Tensor]
  }

  def from(a: Array[_ <: Any]): Tensor = {
    val t = Tensor.ofShape(NdArray.shape(a): _*)

    def fill(a: Array[_ <: Any], ix: List[Int]): Unit = a.head match {
      case arr: Array[_] =>
        for (i <- a.indices) fill(arr, ix :+ i)
      case _ =>
        for (i <- a.indices) t += (NdArray.convert(a(i)), ix :+ i)
    }

    fill(a, List.empty[Int])
    t
  }
}

object NdArray {
  def convert(v: Any): Complex = v match {
    case r: Double => r + 0.i
    case cc: complex.Complex => cc.getReal + cc.getImaginary.i
  }

  def shape(a: Array[_ <: Any]): List[Int] = a.head match {
    case arr: Array[_] => a.length :: shape(arr)
    case _ => List(a.length)
  }

  def pad(a: Array[_ <: Any]): Array[_ <: Any] = {
    def limit(x: Int): Int =
      if (a.length <= x) x else limit(2 * x)

    padTo(a, limit(1))
  }

  def padTo(a: Array[_ <: Any], max: Int): Array[_ <: Any] =
    a ++ Tensor.ofShape(max - a.length :: shape(a).tail: _*).toArray
}

case class Complex(re: Double, im: Double)
  extends complex.Complex(re, im) with TensorLike {

  override def toString: String = Complex.f.format(this)

  def i = Complex(0, this.getReal) // allows for e.g. 1 + 2.i

  def toArray = List(this).toArray

  def toRealArray = List(this.getReal).toArray

  def +(that: Complex): Complex = this add that

  def *(that: Complex): Complex = this multiply that
}

object Complex {
  val f = new complex.ComplexFormat

  def sqrt(c: Complex): Complex = c.pow(0.5)
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
    (u zip v).map(p => p._1 * p._2).reduce(_ + _)
}

trait FourierTransform extends Transform {
  private def mdfft = new FastFourierTransformer(DftNormalization.STANDARD)
    .mdfft(_, TransformType.FORWARD)

  private def imdfft = new FastFourierTransformer(DftNormalization.STANDARD)
    .mdfft(_, TransformType.INVERSE)

  def transform(trajectory: Trajectory): Concept = {
    val arr = trajectory.tensor.toArray
    val input = NdArray.pad(arr)
    val freq = mdfft(input).asInstanceOf[Array[_]]
    Concept(Tensor.from(freq))
  }

  def inverse(concept: Concept): Trajectory = {
    val arr = concept.tensor.toArray
    val input = NdArray.pad(arr)
    val time = imdfft(input).asInstanceOf[Array[_]]
    Trajectory.fromTensor(Tensor.from(time))
  }
}

object Runner extends App {
  val t1 = Tensor.ofShape(5, 5, 5, 5)
  val t2 = Tensor.ofShape(5, 5, 5, 5)
  val t3 = Tensor.ofShape(5)
  val congruent = t1 congruent t2
  t1 += (1 + 1.i, List(1, 1, 1, 1))
  val c = t1(1, 1, 1, 1)
  val a = t3.toArray
  val notation = 1 + 2.i
}