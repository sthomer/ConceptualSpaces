package space

import java.util.UUID

import Implicits._

trait Space extends InnerProduct with Transform
  with Categorization with Segmentation { // make immutable??

  this: InnerProduct with Transform
    with Categorization with Segmentation =>

  //  var concepts: Vector[Concept] = Vector.empty
  //  var trajectories: Vector[Trajectory] = Vector.empty // unnecessary?

  def +=(concept: Concept): Unit = {
    attach(concept)
    feed(concept)
  }
}

object Concept {
  def combine(cs: Vector[Concept]): Concept = Concept(cs
    .map(c => c.tensor)
    .reduce[TensorLike]((acc, t) => acc + t)
    / cs.length)

  val empty = Concept(Complex())
}

case class Concept(tensor: TensorLike) {
  var label: String = UUID.randomUUID().toString.take(5)

  override def toString: String = label
//
//  override def equals(that: Any): Boolean = that match {
//    case that: Concept => label == that.label
//    case _ => false
//  }
//
//  override def hashCode: Int = label.hashCode()

}

case class Trajectory(concepts: Vector[Concept] = Vector.empty) {
  lazy val tensor = Tensor(concepts map (_.tensor))
}

class RawEuclidianSpace extends Space
  with Euclidian with FastFourierTransform
  with NoiseRadius with RisingEntropy with LinearFill

class EuclidianSpace extends Space
  with Euclidian with FastFourierTransform
  with VarianceRadius with RisingEntropy with LinearFill
