package space

import Implicits._

trait InnerProduct {
  def distance(a: Concept, b: Concept): Double

  def norm(c: Concept): Double
}

trait Euclidian extends InnerProduct {
  def norm(c: Concept): Double =
    (c.tensor.conjugate * c.tensor).sqrt.sum.magnitude

  def distance(a: Concept, b: Concept): Double =
    norm(Concept(a.tensor - b.tensor))

  def normalize(c: Concept): Concept =
    Concept(c.tensor / norm(c))
}
