package space

import java.util.UUID

trait Space extends InnerProduct with Transform {
  this: InnerProduct with Transform =>

  var concepts: Vector[Concept] = Vector.empty

  def add(concept: Concept): Unit = {
    concepts = concepts :+ concept
  }
}

object Concept {
  val empty = Concept(Complex())
}

case class Concept(tensor: TensorLike) {
  var category: String = UUID.randomUUID().toString.take(5)

  override def toString: String = category
}

case class Trajectory(concepts: Vector[Concept] = Vector.empty) {
  lazy val tensor = Tensor(concepts map (_.tensor))
}

class EuclidianSpace extends Space
  with Euclidian with FastFourierTransform
