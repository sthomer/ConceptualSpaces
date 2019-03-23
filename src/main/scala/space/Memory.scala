package space

/*
For now, assume there is only one dimension per abstraction layer
  and there is only one inner product.
Therefore, there are at most two partitions for each dimension,
  the categorization partition and the segmentation partition.
*/
case class Memory(perception: Dimension = Dimension.empty,
//                  abstractions: Vector[Dimension] = Vector.empty,
                  categorizations: Map[Dimension, Partition] =
                  Map.empty.withDefaultValue(Partition.empty),
                  segmentations: Map[Dimension, Partition] =
                  Map.empty.withDefaultValue(Partition.empty)) {

  def categorize(concept: Concept,
                 dimension: Dimension,
                 fixedPoint: Boolean = false): Memory =
    dimension.categorize(concept, categorizations(dimension)) match {
      case None if fixedPoint =>
        copy(categorizations =
          categorizations + (dimension -> categorizations(dimension)))
      case None if !fixedPoint =>
        segment(concept, dimension, fixedPoint = true)
      case Some(Partition(_, superior, _)) =>
        categorize(superior.concepts.last, superior)
    }

  def segment(concept: Concept,
              dimension: Dimension,
              fixedPoint: Boolean = false): Memory =
    dimension.segment(concept, segmentations(dimension)) match {
      case None if fixedPoint =>
        copy(segmentations =
          segmentations + (dimension -> segmentations(dimension)))
      case None if !fixedPoint =>
        categorize(concept, dimension, fixedPoint = true)
      case Some(Partition(_, superior, _)) =>
        segment(superior.concepts.last, superior)
    }

  def perceive(concept: Concept): Memory =
    categorize(concept, perception :+ concept) // or start with segment?
}

object Dimension {
  def empty: Dimension = Dimension()
}

// Later, space could be library of spaces i.e. Vector[Space]
case class Dimension(private val id: Int = hashCode,
                     space: Space = new EuclidianSpace,
                     concepts: Vector[Concept] = Vector.empty) {

  override def equals(that: Any): Boolean = that match {
    case that: Dimension => (that canEqual this) && id == that.id
    case _ => false
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Dimension]

  override def hashCode: Int = id

  def :+(concept: Concept): Dimension = {
    copy(concepts = concepts :+ concept)
  }

  // Superior is equal rank tensor to inferior
  def categorize(concept: Concept, partition: Partition): Option[Partition] = {
    space.categorize(concept) match {
      case None => None
      case Some(category) =>
        val s = partition.subtend
        Some(partition.copy(
          inferior = this,
          superior = partition.superior :+ category,
          subtend = s + (category -> (s(category) :+ concept).distinct)))
    }
  }

  // Superior is higher rank tensor than inferior
  def segment(concept: Concept, partition: Partition): Option[Partition] = {
    space.segment(concept) match {
      case None => None
      case Some(segment) =>
        val s = partition.subtend
        Some(partition.copy(
          inferior = this,
          superior = partition.superior :+ segment,
          subtend = s + (segment -> (s(segment) :+ concept))))
    }
  }
}

object Partition {
  def empty: Partition = Partition()
}

// subtend: Superior -> Inferiors
// Should there be different partition types for Categorization and Segmentation?
// i.e. Categorization = Concept -> Set; Segmentation = Concept -> Vector
case class Partition(inferior: Dimension = Dimension.empty,
                     superior: Dimension = Dimension.empty,
                     subtend: Map[Concept, Vector[Concept]]
                     = Map.empty.withDefaultValue(Vector.empty))
