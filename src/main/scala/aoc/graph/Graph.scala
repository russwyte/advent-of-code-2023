package aoc.graph

trait GraphTraversal[A]:
  val startNode: A
  def neighbors(node: A): IterableOnce[(A, Int)]

trait UnitNeighbors[A]:
  this: GraphTraversal[A] =>
  def unitNeighbors(node: A): IterableOnce[A]

  override final def neighbors(node: A): IterableOnce[(A, Int)] = unitNeighbors(node).iterator.map(_ -> 1)

trait Distances[A]:
  def distances: collection.Map[A, Int]

  def nodes: collection.Set[A] = distances.keySet

trait Order[A]:
  def nodeOrder: collection.Seq[A]

trait GraphSearch[A] extends GraphTraversal[A]:
  def isTargetNode(node: A, dist: Int): Boolean

trait TargetNode[A]:
  this: GraphSearch[A] =>
  val targetNode: A

  override def isTargetNode(node: A, dist: Int): Boolean = node == targetNode

trait Heuristic[A]:
  this: GraphSearch[A] =>
  def heuristic(node: A): Int

trait Target[A]:
  def target: Option[(A, Int)]

trait GraphComponents[A]:
  def nodes: IterableOnce[A]
  def unitNeighbors(node: A): IterableOnce[A]
