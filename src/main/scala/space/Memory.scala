package space

import java.util.UUID

case class Node(private val id: String = UUID.randomUUID().toString.take(5),
                var concept: Concept = Concept.empty,
                level: Int = 0) {

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

  var dimensions: Vector[Dimension] = Vector(Dimension())

  def dimensionAt(i: Int): Dimension = {
    if (i < dimensions.length) dimensions(i)
    else { // Dimensions are never skipped, so this works
      dimensions = dimensions :+ Dimension()
      dimensions(i)
    }
  }

  var count = 0

  def perceive(concept: Concept): Unit = {
    val current = Node(concept = concept)
    current.prev = root
    root.next = current
    current.prev = root

    append(current)

    root = current

    println(count)
    count = count + 1
  }

  def append(node: Node): Option[Node] = {
    dimensionAt(node.level).add(node.concept) match {
      case None => None
      case Some(abstraction) =>
        val superior = Node(concept = abstraction, level = node.level + 1)
        node.prev.get.up = superior
        subtend(node.prev, superior)
        append(superior)
    }
  }

  // TODO: this is connecting backwards; does it matter?
  def subtend(node: Option[Node], superior: Node): Unit =
    node match {
      case None =>
      case Some(node) => node.prev match {
        case None => // Beginning of abstraction layer
          superior.down = node
        case Some(node: Node) => node.up match {
          case None =>
            subtend(node.prev, superior)
          case Some(prevUp: Node) =>
            superior.prev = Some(prevUp)
            superior.down = node
        }
      }
    }


  // QoL Helpers

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

}

