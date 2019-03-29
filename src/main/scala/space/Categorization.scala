package space

import Implicits._

trait Categorization extends InnerProduct {

  var concepts: Vector[Concept]

  def label(concept: Concept): Unit
}

trait VarianceRadius extends Categorization {

  // TODO: Check cross-entropy reduction + convexity before clumping

  var concepts: Vector[Concept] = Vector.empty
  //  var categories: Set[Set[Concept]] = Set.empty
  var M: TensorLike = 0 // placeholder
  var S: TensorLike = 0 // placeholder

  def label(concept: Concept): Unit = {
    concepts = concepts :+ concept
    val x = concept.tensor
    val Mk = M + ((x - M) / concepts.length)
    val Sk = S + ((x - M) * (x - Mk))
    M = Mk
    S = Sk
    val v = if (concepts.length == 1) S else S / (concepts.length - 1)
    val sd = Concept(v.sqrt)
    val r = norm(sd) * 0.68 // i.e. 50%
    val members = concepts filter (distance(_, concept) < r) toSet
    val categories = members flatMap (_.categories)
    concept.categories = categories
  }
  // Think about greedy labeling to avoid checking all concepts every time

}
