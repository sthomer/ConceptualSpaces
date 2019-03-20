package space

class Info

class Model {
  import scala.collection.mutable.Map
  val es: Map[(Concept, Concept), Int] = Map().withDefaultValue(0)
  val is: Map[Concept, Int] = Map().withDefaultValue(0)

  def meanInfo: Double =
    is.keys.map(c => info(c)).sum / is.size.toDouble

  def info(concept: Concept): Double =
    - math.log(is(concept) / is.values.sum)

  def entropy(concept: Concept): Double = {
    val counts = es.filterKeys(_._1 == concept).values
    val total = counts.sum.toDouble
    val probs = counts.map(n => n / total)
    if (probs.size == 1) - probs.head * math.log(probs.head)
    else probs.reduce((acc, p) => acc - p * math.log(p))
  }

  def add(current: Concept, next: Concept): Unit = {
    es += (current, next) -> (es((current, next)) + 1)
    is += current -> (is(current) + 1)
  }
}
