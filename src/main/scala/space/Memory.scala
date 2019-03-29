package space

import java.util.UUID

case class Node(private val id: String = UUID.randomUUID().toString.take(5),
                var concept: Concept = Concept.empty,
                level: Int = 0) {

  var set: Set[Concept] = Set(concept)
  //  private var _space: Space = new EuclidianSpace
  private var _prev: Option[Node] = None
  private var _next: Option[Node] = None
  private var _up: Option[Node] = None
  private var _down: Option[Node] = None

  //  def space: Space = _space
  //
  //  def space_=(space: Space): Unit = {
  //    _space = space
  //    _space.feed(concept)
  //  }

  override def toString: String = concept.label

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

  //  def up: Node = _up match {
  //    case None =>
  //      val category = Node(concept = concept, level = level + 1)
  //      category.down = this
  //      _up = Some(category)
  //      category
  //    case Some(node) =>
  //      node
  //  }
  //
  //  def up_=(node: Node): Unit = _up = Some(node)
  //
  //  def down: Node = _down match {
  //    case None => // This should never happen?
  //      val node = Node(concept = concept, level = level - 1)
  ////      node.space = this.space
  //      node.up = this
  //      _down = Some(node)
  //      node
  //    case Some(node) =>
  //      node
  //  }
  //
  //  def down_=(node: Node): Unit = _down = Some(node)
}

object Memory {
  def make(concept: Concept): Memory = {
    val mem = Memory()
    val root = Node(concept = concept)
    mem.dimensionAt(root.level).feed(concept)
    mem.categorize(root)
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

  import scala.collection.mutable

  // For now, all mappings in same map (this is technically unsafe,
  //   but it most likely won't matter b/c concepts are so sparse
  // superior -> inferior
  val categoryPartition: mutable.Map[Concept, Set[Concept]] =
  mutable.Map.empty.withDefaultValue(Set.empty)

  var dimensions: Vector[Space] = Vector(new RawEuclidianSpace)

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

    dimensionAt(current.level).feed(concept)
    categorize(current)

    val category = current.up.get
    val space = dimensionAt(category.level)
    space.attachAll(space.concepts)
    segment(category)

    root = current
  }

  def categorize(node: Node): Unit = {
    val prevCategory = node.prevCategory
    dimensionAt(node.level).catset(node.concept) match {
      case (cat, set) =>

        val category = Node(concept = cat, level = node.level + 1)
        dimensionAt(category.level).feed(category.concept)
        category.set = set
        category.prev = prevCategory
        prevCategory foreach {
          _.next = category
        }
        category.down = node
        node.up = category

        categoryPartition += cat -> set
        categoryPartition foreach { case (c, s) => c.label = s.head.label }

      // Just categorizing (small) noise after first categorization
      // if (node.concept != cat) categorize(category)
    }
  }


  // I think this is connecting backwards
  def segment(node: Node): Unit = {
    dimensionAt(node.level).segopt(node.concept) match {
      case None =>
      case Some(concept) =>
        val category = Node(concept = concept, level = node.level + 1)
        dimensionAt(category.level).feed(category.concept)
        node.prev.get.up = category
        subtend(node.prev, category)

      // This probably does nothing...
      //        segment(category)
    }
  }

  def subtend(node: Option[Node], category: Node): Unit =
    node.get.prev match {
      case None => // Beginning of abstraction layer
      case Some(node: Node) => node.up match {
        case None =>
          subtend(node.prev, category)
        case Some(prevUp: Node) =>
          category.prev = Some(prevUp)
          category.down = node
      }
    }

  //        node.up = Some(category)
  //        category.down = Some(category.down match {
  //          case None => Vector(node)
  //          case Some(v) => node +: v
  //        })
}


