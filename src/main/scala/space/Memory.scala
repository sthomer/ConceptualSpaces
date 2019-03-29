package space

import java.util.UUID

case class Node(private val id: String = UUID.randomUUID().toString.take(5),
                var concept: Concept = Concept.empty,
                level: Int = 0) {

  var set: Set[Concept] = Set(concept)
  private var _prev: Option[Node] = None
  private var _next: Option[Node] = None
  private var _up: Option[Node] = None
  private var _down: Option[Node] = None

  override def toString: String = concept.toString

  override def equals(that: Any): Boolean = that match {
    case that: Node => id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode()

  def prev: Option[Node] = _prev

  def prev_=(node: Node): Unit = _prev = Some(node)

  def prev_=(node: Option[Node]): Unit = _prev = node

  def next: Option[Node] = _next

  def next_=(node: Node): Unit = _next = Some(node)

  def up: Option[Node] = _up

  def up_=(node: Node): Unit = _up = Some(node)

  def down: Option[Node] = _down

  def down_=(node: Node): Unit = _down = Some(node)

  def prevCategory: Option[Node] = for {
    prevNode <- prev
    up <- prevNode.up
  } yield up

}

object Memory {
  def make(concept: Concept): Memory = {
    val mem = Memory()
    val root = Node(concept = concept)
    mem.root = root
    mem
  }
}

case class Memory(var root: Node = Node()) {

  def collect(node: Node): Vector[Node] = node.prev match {
    case None => Vector(node)
    case Some(prev: Node) => collect(prev) :+ node
  }

  def collectN(n: Int, node: Node): Vector[Option[Node]] = node.prev match {
    case None => Vector(upN(n, node))
    case Some(prev: Node) => collectN(n, prev) :+ upN(n, prev)
  }

  def upN(n: Int, node: Node): Option[Node] = {
    if (n == 0) Some(node)
    else node.up match {
      case None => None
      case Some(node: Node) => upN(n - 1, node)
    }
  }

  var dimensions: Vector[Space] = Vector(new EuclidianSpace)

  def dimensionAt(i: Int): Space = {
    if (i < dimensions.length) dimensions(i)
    else { // Dimensions are never skipped, so this works
      dimensions = dimensions :+ new EuclidianSpace
      dimensions(i)
    }
  }

  def perceive(concept: Concept): Unit = {
    val current = Node(concept = concept)
    current.prev = root
    root.next = current
    current.prev = root

    val space = dimensionAt(current.level)
    space label concept
    space attachAll space.concepts
    segment(current)

    root = current
  }

  // I think this is connecting backwards
  def segment(node: Node): Unit = {
    dimensionAt(node.level).segopt(node.concept) match {
      case None =>
      case Some(concept) =>
        val category = Node(concept = concept, level = node.level + 1)
        dimensionAt(category.level) label concept
        node.prev.get.up = category
        subtend(node.prev, category)
    }
  }

  def subtend(node: Option[Node], category: Node): Unit =
    node match {
      case None =>
      case Some(node) => node.prev match {
        case None => // Beginning of abstraction layer
        case Some(node: Node) => node.up match {
          case None =>
            subtend(node.prev, category)
          case Some(prevUp: Node) =>
            category.prev = Some(prevUp)
            category.down = node
        }
      }
    }
}

