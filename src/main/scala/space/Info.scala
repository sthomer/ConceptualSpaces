package space

class InfoModel {

  import scala.collection.mutable

  val es: mutable.Map[(String, String), Int] = mutable.Map().withDefaultValue(0)
  val is: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0)

  def meanInfo: Double =
    is.keys.map(c => info(c)).sum

  def info(label: String): Double =
    -math.log(is(label) / is.values.sum)

  def entropy(concept: Concept): Double = {
    val counts = es filterKeys { case (curr, _) => curr == concept.category } values
    val total = counts.sum.toDouble
    val probs = counts map { n => n / total }
    if (probs.isEmpty) 0
    else if (probs.size == 1) -probs.head * math.log(probs.head)
    else probs.reduce((acc, p) => acc - p * math.log(p))
  }

  def add(previous: Concept, current: Concept): Unit = {
    is += current.category -> (is(current.category) + 1)
    es += (previous.category, current.category) -> (es(previous.category, current.category) + 1)
  }
}
