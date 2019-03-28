package space

import java.util.UUID

case class Node(private val id: String = UUID.randomUUID().toString.take(5),
                var concept: Concept = Concept.empty) {

  var set: Set[Concept] = Set(concept)
  private var _space: Space = new EuclidianSpace
  private var _prev: Option[Node] = None
  private var _next: Option[Node] = None
  private var _up: Option[Node] = None
  private var _down: Option[Node] = None

  def space: Space = _space

  def space_=(space: Space): Unit = {
    _space = space
    _space.feed(concept)
  }

  override def toString: String = id

  override def equals(that: Any): Boolean = that match {
    case that: Node => id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode()

  def prev: Option[Node] = _prev

  def prev_=(node: Node): Unit = _prev = Some(node)

  def next: Option[Node] = _next

  def next_=(node: Node): Unit = _next = Some(node)

  def up: Node = _up match {
    case None =>
      val category = Node(concept = concept)
      category.down = this
      _up = Some(category)
      category
    case Some(node) =>
      node
  }

  def up_=(node: Node): Unit = _up = Some(node)

  def down: Node = _down match {
    case None => // This should never happen?
      val node = Node(concept = concept)
      node.space = this.space
      node.up = this
      _down = Some(node)
      node
    case Some(node) =>
      node
  }

  def down_=(node: Node): Unit = _down = Some(node)
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

  import scala.collection.mutable

  // For now, all mappings in same map (this is technically unsafe,
  //   but it most likely won't matter b/c concepts are so sparse
  val catpart: mutable.Map[Concept, Set[Concept]] =
    mutable.Map.empty.withDefaultValue(Set.empty)

  def collect(node: Node): Vector[Option[Node]] = node.prev match {
    case None => Vector(Some(node))
    case Some(prev: Node) => collect(prev) :+ Some(node)
  }

  def perceive(concept: Concept): Unit = {
    val current = Node(concept = concept)
    current.space = root.space
    current.prev = root
    root.next = current
    current.prev = root

    categorize(current)
    //    segment(current)

    root = current
  }

  // Currently, single shot categorization
  def categorize(node: Node): Unit = {
    val prevCategory = node.prev.get.up
    node.space.catset(node.concept) match {
      case (cat, set) =>
        catpart += cat -> set
        val category = Node(concept = cat)
        category.space = prevCategory.space
        category.set = set
        category.prev = prevCategory
        prevCategory.next = category
        category.down = node
        node.up = category
    }
  }

  //  def replace(node: Node, set: Set[Concept]): Unit = {
  //    val excluded = set.flatMap(catpart(_)) + node.concept
  //    node.space.catset(node.concept, excluded) match {
  //      case (cat, set) =>
  //        catpart += cat -> set
  //        if (node.space.distance(node.concept, cat) > 1E-10) {
  //          node.concept = cat
  //          node.set = set
  //          replace(node, set)
  //        }
  //    }
  //  }

  //  def segment(node: Node): Unit = {
  //    node.space.seg(node.concept) match {
  //      case None =>
  //      case Some(concept) =>
  //        val category = Node(concept = concept)
  //        subtend(node.prev, category)
  //        segment(category)
  //    }
  //  }
  //
  //  def subtend(node: Option[Node], category: Node): Unit = node match {
  //    case None => // Beginning of abstraction layer
  //    case Some(node: Node) => node.up match {
  //      case None =>
  //        node.up = Some(category)
  //        category.down = Some(category.down match {
  //          case None => Vector(node)
  //          case Some(v) => node +: v
  //        })
  //        subtend(node.prev, category)
  //      case Some(prevUp: Node) =>
  //        category.prev = Some(prevUp)
  //    }
  //  }
}

