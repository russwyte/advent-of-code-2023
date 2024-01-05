package aoc.graph
//import scala.collection.mutable
import scala.annotation.tailrec

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

trait Paths[A]:
  extension (l: scala.collection.immutable.LazyList.type)
    def unfold0[A](a: A)(f: A => Option[A]): LazyList[A] =
      LazyList.unfold(a)(a => f(a).map(a => (a, a)))
  def prevNodes: collection.Map[A, A]

  def paths: collection.Map[A, Seq[A]] =
    prevNodes.map((node, _) => node -> (node +: LazyList.unfold0(node)(prevNodes.get)).reverse)

case class Edge[A](a: A, b: A)

object EdmondsKarp:
  case class MaxFLowResult[A](maxFlow: Int, residual: Map[A, Map[A, Int]])
  // basically we treat this algorithm as a function - thus the apply method
  def apply[A](
      edges: Set[Edge[A]],
      startNode0: A,
      targetNode0: A,
  ): MaxFLowResult[A] =
    val initResidual = edges.foldLeft(Map.empty[A, Map[A, Int]]) { (acc, edge) =>
      val Edge(u, v) = edge
      acc
        .updated(u, acc.getOrElse(u, Map.empty).updated(v, 1))
        .updated(v, acc.getOrElse(v, Map.empty).updated(u, 1))
    }

    @tailrec
    def loop(maxFlow: Int, residual: Map[A, Map[A, Int]]): MaxFLowResult[A] =
      val graphSearch = new GraphSearch[A] with UnitNeighbors[A] with TargetNode[A]:
        override val startNode: A = startNode0

        override def unitNeighbors(node: A): IterableOnce[A] =
          for
            (toNode, r) <- residual(node)
            if r > 0
          yield toNode

        override val targetNode: A = targetNode0

      val searchResult = BFS.searchPaths(graphSearch)
      if searchResult.target.isEmpty then MaxFLowResult(maxFlow, residual)
      else
        val targetPath = searchResult.paths(targetNode0)
        val pathEdges  = targetPath lazyZip targetPath.tail
        val pathFlow   = pathEdges.map(residual(_)(_)).min
        val newRes = pathEdges.foldLeft(residual) { (acc, edge) =>
          val (u, v) = edge
          acc
            .updated(u, acc(u).updated(v, acc(u)(v) - pathFlow))
            .updated(v, acc(v).updated(u, acc(v)(u) + pathFlow))
        }
        loop(maxFlow + pathFlow, newRes)
      end if
    end loop
    loop(0, initResidual)
  end apply
end EdmondsKarp
