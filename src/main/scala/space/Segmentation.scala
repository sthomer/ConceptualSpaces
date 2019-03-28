package space

trait Segmentation extends InnerProduct with Interpolation with Transform {

  // Returns the superior concept of the just finished, or None
  def seg(concept: Concept): Option[Concept] = None

  val m = new Model
  var segments: Vector[Vector[Concept]] = Vector.empty
  var ongoing: Vector[Concept] = Vector.empty
  var previous: Concept = Concept.empty // placeholder

  def attach(concept: Concept): Unit = m.add(previous, concept)

  // inferior -> superior
  def segment(concept: Concept): Option[(Concept, Vector[Concept])] = {
    attach(concept)
    var mapping: Option[(Concept, Vector[Concept])] = None
    if (detect(previous, concept)) {
      segments = segments :+ ongoing
      mapping = Some(transform(Trajectory(interpolate(ongoing))) -> ongoing)
      ongoing = Vector.empty
    }
    ongoing = ongoing :+ concept
    previous = concept
    mapping
  }

  def segpart(concept: Concept): Map[Concept, Vector[Concept]] = {
    m.add(previous, concept)
    if (detect(previous, concept)) {
      segments = segments :+ ongoing
      ongoing = Vector.empty
    }
    ongoing = ongoing :+ concept
    previous = concept

    segments.map(segment => {
      val concept = Concept.combine(segment) // i.e. Interpolate + Transform
      concept -> segment
    }).toMap
  }

  def detect(previous: Concept, current: Concept): Boolean
}

trait RisingEntropy extends Segmentation with LinearFill {

  def detect(previous: Concept, current: Concept): Boolean =
    m.entropy(previous) < m.entropy(current)


  def add(previous: Concept, current: Concept): Unit = m.add(previous, current)

  def chop(concepts: Vector[Concept]): Unit = {
    (Concept.empty +: concepts).sliding(2).foreach({
      case Vector(previous: Concept, current: Concept) =>
        add(previous, current)
        if (detect(previous, current)) {
          segments = segments :+ ongoing
          ongoing = Vector.empty
        }
        ongoing = ongoing :+ current
    })
    segments = segments :+ ongoing
    ongoing = Vector.empty
  }

  def segmentize: Vector[Concept] =
    segments.flatMap(segment => {
      val concept = Concept.combine(segment) // i.e. Interpolate + Transform
      segment.map(c => concept)
    })
}

