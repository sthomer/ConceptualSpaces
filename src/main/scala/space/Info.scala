package space

class InfoModel {
  import scala.collection.mutable.Map
  val es: Map[(String, String), Int] = Map().withDefaultValue(0)
  val is: Map[String, Int] = Map().withDefaultValue(0)

  def meanInfo: Double =
    is.keys.map(c => info(c)).sum

  def info(label: String): Double =
    - math.log(is(label) / is.values.sum)

  def entropy(label: String): Double = {
    val counts = es.filterKeys(_._1 == label).values
    val total = counts.sum.toDouble
    val probs = counts.map(n => n / total)
    if (probs.isEmpty) 0
    else if (probs.size == 1) - probs.head * math.log(probs.head)
    else probs.reduce((acc, p) => acc - p * math.log(p))
  }

  def add(previous: String, current: String): Unit = {
    es += (previous, current) -> (es((previous, current)) + 1)
    is += current -> (is(current) + 1)
  }
}

//  val es: Map[(Concept, Concept), Int] = Map().withDefaultValue(0)
//  val is: Map[Concept, Int] = Map().withDefaultValue(0)
//
//  def meanInfo: Double =
//    is.keys.map(c => info(c)).sum
//
//  def info(concept: Concept): Double =
//    - math.log(is(concept) / is.values.sum)
//
//  def entropy(concept: Concept): Double = {
//    val counts = es.filterKeys(_._1 == concept).values
//    val total = counts.sum.toDouble
//    val probs = counts.map(n => n / total)
//    if (probs.isEmpty) 0
//    else if (probs.size == 1) - probs.head * math.log(probs.head)
//    else probs.reduce((acc, p) => acc - p * math.log(p))
//  }
//
//  def add(previous: Concept, current: Concept): Unit = {
//    es += (previous, current) -> (es((previous, current)) + 1)
//    is += current -> (is(current) + 1)
//  }
