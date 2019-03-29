package space

import Implicits._

trait Categorization extends InnerProduct {

  var concepts: Vector[Concept]

  def feed(concept: Concept)

  def catset(concept: Concept): (Concept, Set[Concept])
}

trait VarianceRadius extends Categorization {

  // TODO: Check cross-entropy reduction + convexity before clumping

  def catset(concept: Concept): (Concept, Set[Concept]) = {
    val set = clumps
      .filter(cl => cl.head.label != concept.label)
      .find(cl => cl(concept))
      .getOrElse(Set(concept))
    propagateLabels()
    val category = center(set)
    category -> set
  }

  var concepts: Vector[Concept] = Vector.empty
  var clumps: Vector[Set[Concept]] = Vector.empty
  var distincts: Set[Concept] = Set.empty
  var M: TensorLike = 0 // placeholder
  var S: TensorLike = 0 // placeholder

  def sd: TensorLike =
    if (distincts.size == 1) S
    else (S / (distincts.size - 1)).sqrt

  def radius: Double = norm(Concept(sd)) * 0.68

  def clump(target: Concept): Set[Concept] = {
    val r = radius
    (if (norm(target) > 0.05 * r)
      clumps.filter(cl => cl.head.label != target.label)
        .filter(cl => cl.exists(c =>
          norm(c) > 0.05 * r && distance(c, target) < r
        )).flatten.toSet
    else Set.empty[Concept]) + target
  }

  def feed(concept: Concept): Unit = {
    concepts = concepts :+ concept
    val x = concept.tensor
    if (!distincts(concept) && concept.tensor.finite) {
      distincts = distincts + concept
      val Mk = M + ((x - M) / distincts.size)
      val Sk = S + ((x - M) * (x - Mk))
      M = Mk
      S = Sk
    }

    val cl = clump(concept)
    clumps = (clumps :+ cl).map(clump =>
      if ((clump & cl) != Set.empty) clump | cl else clump).distinct
  }

  def propagateLabels(): Unit =
    clumps foreach { clump =>
      val label = clump.head.label
      clump foreach { concept =>
        concept.label = label
      }
    }

  def fill(concepts: Vector[Concept]): Unit = concepts foreach (c => feed(c))

  def center(members: Set[Concept]): Concept = {
    val category = Concept(members
      .map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => acc + t) / members.size)
    category.label = members.head.label
    category
  }

  def categorize: Vector[Concept] = {
    val centers = clumps.map(cl => cl -> center(cl)).toMap
    concepts.flatMap(c => clumps.find(cl => cl(c)))
      .map(cl => centers(cl))
  }
}

// No 5% cutoff vs VarianceRadius
trait NoiseRadius extends VarianceRadius {
  override def clump(target: Concept): Set[Concept] = {
    val r = radius
    clumps
      .filter(cl => cl.exists(c => distance(c, target) < r))
      .flatten.toSet + target
  }

  override def feed(concept: Concept): Unit = {
    concepts = concepts :+ concept
    val x = concept.tensor
    if (!distincts(concept) && concept.tensor.finite) {
      distincts = distincts + concept
      val Mk = M + ((x - M) / distincts.size)
      val Sk = S + ((x - M) * (x - Mk))
      M = Mk
      S = Sk
    }

    val cl = clump(concept)
    clumps = (clumps :+ cl).map(clump =>
      if ((clump & cl) != Set.empty) clump | cl else clump).distinct
  }
}
