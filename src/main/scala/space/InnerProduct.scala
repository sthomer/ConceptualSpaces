package space

import Implicits._

trait InnerProduct {
  def distance(a: Concept, b: Concept): BigDecimal

  def norm(c: Concept): BigDecimal
}

trait Euclidian extends InnerProduct {
  def norm(c: Concept): BigDecimal =
    (c.tensor.conjugate * c.tensor).sqrt.total

  def distance(a: Concept, b: Concept): BigDecimal =
    norm(Concept(a.tensor - b.tensor))
}
