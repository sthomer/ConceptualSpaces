package space

class InfoModel {

  import scala.collection.mutable

  val es: mutable.Map[(String, String), Int] = mutable.Map().withDefaultValue(0)
  val is: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0)

  def meanInfo: Double =
    is.keys.map(c => info(c)).sum

  def info(label: String): Double =
    -math.log(is(label) / is.values.sum)

  // What is entropy of concept with multiple categories?
  //   the sum of the entropy of each category
  def entropy(concept: Concept): Double = {
    concept.categories map { category =>
      val counts = es filterKeys { case (curr, _) => curr == category } values
      val total = counts.sum.toDouble
      val probs = counts map { n => n / total }
      if (probs.isEmpty) 0
      else if (probs.size == 1) -probs.head * math.log(probs.head)
      else probs.reduce((acc, p) => acc - p * math.log(p))
    } sum
  }

  def add(previous: Concept, current: Concept): Unit = {
    current.categories foreach { curr =>
      is += curr -> (is(curr) + 1)
    }
    previous.categories flatMap { prev =>
      current.categories map { curr =>
        (prev, curr)
      }
    } foreach {
      case (prev, curr) =>
        es += (prev, curr) -> (es(prev, curr) + 1)
    }
  }
}
