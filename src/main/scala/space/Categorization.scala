package space

import Implicits._

trait Categorization extends InnerProduct {

  def feed(concept: Concept, excluded: Set[Concept] = Set.empty)

  def catset(concept: Concept,
             excluded: Set[Concept] = Set.empty): (Concept, Set[Concept])
}

trait VarianceRadius extends Categorization {

  def catset(concept: Concept,
             excluded: Set[Concept] = Set.empty): (Concept, Set[Concept]) = {
    feed(concept, excluded)
    val set = clumps
      .filter(cl => !cl.subsetOf(excluded) || cl == Set(concept))
      .find(cl => cl(concept)).get
    val category = center(set)
    category -> set
  }

  // TODO: Check cross-entropy reduction + convexity before clumping

  var concepts: Vector[Concept] = Vector.empty
  var clumps: Vector[Set[Concept]] = Vector.empty
  var distincts: Set[Concept] = Set.empty
  var M: TensorLike = 0 // placeholder
  var S: TensorLike = 0 // placeholder

  def sd: TensorLike =
    if (distincts.size == 1) S
    else (S / (distincts.size - 1)).sqrt

  def radius: Double = norm(Concept(sd)) * 0.68

  def clump(target: Concept,
            excluded: Set[Concept] = Set.empty): Set[Concept] = {
    val r = radius
    (if (norm(target) > 0.05 * r)
      clumps.filter(cl => !cl.subsetOf(excluded))
        .filter(cl => cl.exists(c =>
          norm(c) > 0.05 * r && distance(c, target) < r
        )).flatten.toSet
    else Set.empty[Concept]) + target
  }

  def feed(concept: Concept, excluded: Set[Concept] = Set.empty): Unit = {
    concepts = concepts :+ concept
    val x = concept.tensor
    if (!distincts(concept) && concept.tensor.finite) {
      distincts = distincts + concept
      val Mk = M + ((x - M) / distincts.size)
      val Sk = S + ((x - M) * (x - Mk))
      M = Mk
      S = Sk
    }

    val cl = clump(concept, excluded)
    clumps = (clumps :+ cl).map(clump =>
      if ((clump & cl) != Set.empty) clump | cl else clump).distinct
  }

  def fill(concepts: Vector[Concept]): Unit = concepts foreach (c => feed(c))

  def center(members: Set[Concept]): Concept =
    Concept(members
      .map(c => c.tensor)
      .reduce[TensorLike]((acc, t) => acc + t)
      / members.size)

  def categorize: Vector[Concept] = {
    val centers = clumps.map(cl => cl -> center(cl)).toMap
    concepts.flatMap(c => clumps.find(cl => cl(c)))
      .map(cl => centers(cl))
  }
}

// No 5% cutoff vs VarianceRadius
trait NoiseRadius extends VarianceRadius {
  override def clump(target: Concept,
                     excluded: Set[Concept] = Set.empty): Set[Concept] = {
    val r = radius
    clumps.filter(cl => !cl.subsetOf(excluded))
      .filter(cl => cl.exists(c => distance(c, target) < r))
      .flatten.toSet + target
  }
}
