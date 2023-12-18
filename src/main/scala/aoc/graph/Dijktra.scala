package aoc.graph

import scala.collection.mutable

object Dijkstra:

  def traverse[A](graphTraversal: GraphTraversal[A]): Distances[A] =
    val visitedDistance: mutable.Map[A, Int]     = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))
    val visited: mutable.HashSet[A]              = mutable.HashSet.empty

    def enqueue(node: A, dist: Int): Unit =
      if !visited(node) then
        toVisit.enqueue((dist, node))
        visited += node

    enqueue(graphTraversal.startNode, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      visitedDistance(node) = dist

      def goNeighbor(newNode: A, distDelta: Int): Unit =
        val newDist = dist + distDelta
        enqueue(newNode, newDist)

      graphTraversal.neighbors(node).iterator.foreach((goNeighbor _).tupled)
    end while

    new Distances[A]:
      override def distances: collection.Map[A, Int] = visitedDistance
  end traverse

  def search[A](graphSearch: GraphSearch[A]): Distances[A] with Target[A] =
    val visitedDistance: mutable.Map[A, Int]     = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, A)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))
    val visited: mutable.HashSet[A]              = mutable.HashSet.empty

    def enqueue(node: A, dist: Int): Unit =
      if !visited(node) then
        toVisit.enqueue((dist, node))
        visited += node

    enqueue(graphSearch.startNode, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      visitedDistance(node) = dist

      if graphSearch.isTargetNode(node, dist) then
        return new Distances[A] with Target[A]:
          override def distances: collection.Map[A, Int] = visitedDistance
          override def target: Option[(A, Int)]          = Some(node -> dist)

      def goNeighbor(newNode: A, distDelta: Int): Unit =
        val newDist = dist + distDelta
        enqueue(newNode, newDist)

      graphSearch.neighbors(node).iterator.foreach((goNeighbor _).tupled)
    end while

    new Distances[A] with Target[A]:
      override def distances: collection.Map[A, Int] = visitedDistance
      override def target: Option[(A, Int)]          = None
  end search
end Dijkstra
