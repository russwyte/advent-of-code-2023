package aoc.graph

import scala.annotation.tailrec
import scala.collection.mutable

object BFS:
  def traverse[A](graphTraversal: GraphTraversal[A] with UnitNeighbors[A]): Distances[A] =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, A)]     = mutable.Queue.empty

    def enqueue(node: A, dist: Int): Unit =
      toVisit.enqueue((dist, node))

    enqueue(graphTraversal.startNode, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      if !visitedDistance.contains(node) then
        visitedDistance(node) = dist

        def goNeighbor(newNode: A): Unit =
          if !visitedDistance.contains(newNode) then // avoids some unnecessary queue duplication but not all
            val newDist = dist + 1
            enqueue(newNode, newDist)

        graphTraversal.unitNeighbors(node).iterator.foreach(goNeighbor)
    end while

    new Distances[A]:
      override def distances: collection.Map[A, Int] = visitedDistance
  end traverse

  def search[A](graphSearch: GraphSearch[A] with UnitNeighbors[A]): Distances[A] with Target[A] =
    val visitedDistance: mutable.Map[A, Int] = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, A)]     = mutable.Queue.empty

    def enqueue(node: A, dist: Int): Unit =
      toVisit.enqueue((dist, node))

    enqueue(graphSearch.startNode, 0)

    while toVisit.nonEmpty do
      val (dist, node) = toVisit.dequeue()
      if !visitedDistance.contains(node) then
        visitedDistance(node) = dist

        if graphSearch.isTargetNode(node, dist) then
          return new Distances[A] with Target[A]:
            override def distances: collection.Map[A, Int] = visitedDistance

            override def target: Option[(A, Int)] = Some(node -> dist)

        def goNeighbor(newNode: A): Unit =
          if !visitedDistance.contains(newNode) then
            val newDist = dist + 1
            enqueue(newNode, newDist)

        graphSearch.unitNeighbors(node).iterator.foreach(goNeighbor)
      end if
    end while

    new Distances[A] with Target[A]:
      override def distances: collection.Map[A, Int] = visitedDistance

      override def target: Option[(A, Int)] = None
  end search

  def searchPaths[A](graphSearch: GraphSearch[A] with UnitNeighbors[A]): Distances[A] with Paths[A] with Target[A] =
    val visitedDistance: mutable.Map[A, Int]        = mutable.Map.empty
    val prevNode: mutable.Map[A, A]                 = mutable.Map.empty
    val toVisit: mutable.Queue[(Int, Option[A], A)] = mutable.Queue.empty

    def enqueue(oldNode: Option[A], node: A, dist: Int): Unit =
      toVisit.enqueue((dist, oldNode, node))

    enqueue(None, graphSearch.startNode, 0)

    while toVisit.nonEmpty do
      val (dist, oldNode, node) = toVisit.dequeue()
      if !visitedDistance.contains(node) then
        visitedDistance(node) = dist
        for oldNode <- oldNode do prevNode(node) = oldNode

        if graphSearch.isTargetNode(node, dist) then
          return new Distances[A] with Paths[A] with Target[A]:
            override def distances: collection.Map[A, Int] = visitedDistance

            override def prevNodes: collection.Map[A, A] = prevNode

            override def target: Option[(A, Int)] = Some(node -> dist)

        def goNeighbor(newNode: A): Unit =
          if !visitedDistance.contains(newNode) then // avoids some unnecessary queue duplication but not all
            val newDist = dist + 1
            enqueue(Some(node), newNode, newDist)

        graphSearch.unitNeighbors(node).iterator.foreach(goNeighbor)
      end if
    end while

    new Distances[A] with Paths[A] with Target[A]:
      override def distances: collection.Map[A, Int] = visitedDistance

      override def prevNodes: collection.Map[A, A] = prevNode

      override def target: Option[(A, Int)] = None
  end searchPaths

  def components[A](graphComponents: GraphComponents[A]): collection.Set[collection.Set[A]] =

    def bfs(start: A): collection.Set[A] =

      val graphTraversal = new GraphTraversal[A] with UnitNeighbors[A]:
        override val startNode: A = start

        override def unitNeighbors(node: A): IterableOnce[A] = graphComponents.unitNeighbors(node)

      BFS.traverse(graphTraversal).nodes

    def bfsGroups(nodes: Set[A]): Set[collection.Set[A]] =
      if nodes.isEmpty then Set.empty
      else
        val startNode = nodes.head
        val group     = bfs(startNode)
        val restNodes = nodes -- group
        bfsGroups(restNodes) + group

    bfsGroups(graphComponents.nodes.iterator.toSet)
  end components
end BFS
