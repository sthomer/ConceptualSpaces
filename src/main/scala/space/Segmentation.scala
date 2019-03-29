package space

trait Segmentation extends InnerProduct with Interpolation with Transform {

  // Returns the superior concept of the just finished segment, or None
  def segopt(concept: Concept): Option[Concept] = {
    var category: Option[Concept] = None
    if (detect(previous, concept)) {
      segments = segments :+ ongoing
      category = Some(transform(Trajectory(ongoing)))
//      category = Some(transform(Trajectory(interpolate(ongoing))))
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

  // Naive update replays all nodes
  def attachAll(concepts: Vector[Concept]): Unit = {
    model = new InfoModel
    var previous: Concept = Concept.empty // placeholder
    concepts foreach { concept =>
      model.add(previous, concept)
      previous = concept
    }
  }

  def detect(previous: Concept, current: Concept): Boolean
}

trait RisingEntropy extends Segmentation with LinearFill {

  def detect(previous: Concept, current: Concept): Boolean = {
    println(model.entropy(current))
    model.entropy(previous) < model.entropy(current)
  }


}

