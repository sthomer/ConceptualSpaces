package space

trait Segmentation extends InnerProduct with Interpolation with Transform {

  // Returns the superior concept of the just finished segment, or None
  def segopt(concept: Concept): Option[Concept] = {
    var category: Option[Concept] = None
    if (detect(previous, concept)) {
      segments = segments :+ ongoing
      category = Some(transform(Trajectory(interpolate(ongoing))))
      ongoing = Vector.empty
    }
    ongoing = ongoing :+ concept
    previous = concept
    category
  }

  var model: InfoModel = new InfoModel
  var segments: Vector[Vector[Concept]] = Vector.empty
  var ongoing: Vector[Concept] = Vector.empty
  var previous: Concept = Concept.empty // placeholder

  def attach(concept: Concept): Unit = model.add(previous.label, concept.label)

  // Naive update replays all nodes
  def attachAll(concepts: Vector[Concept]): Unit = {
    model = new InfoModel
    var previous: Concept = Concept.empty // placeholder
    concepts foreach { concept =>
      model.add(previous.label, concept.label)
      previous = concept
    }
  }

  def detect(previous: Concept, current: Concept): Boolean
}

trait RisingEntropy extends Segmentation with LinearFill {

  def detect(previous: Concept, current: Concept): Boolean = {
    println(model.entropy(current.label))
    model.entropy(previous.label) < model.entropy(current.label)
  }


//  def add(previous: Concept, current: Concept): Unit = m.add(previous, current)
//
//  def chop(concepts: Vector[Concept]): Unit = {
//    (Concept.empty +: concepts).sliding(2).foreach({
//      case Vector(previous: Concept, current: Concept) =>
//        add(previous, current)
//        if (detect(previous, current)) {
//          segments = segments :+ ongoing
//          ongoing = Vector.empty
//        }
//        ongoing = ongoing :+ current
//    })
//    segments = segments :+ ongoing
//    ongoing = Vector.empty
//  }

  def segmentize: Vector[Concept] =
    segments.flatMap(segment => {
      val concept = Concept.combine(segment) // i.e. Interpolate + Transform
      segment.map(c => concept)
    })
}

