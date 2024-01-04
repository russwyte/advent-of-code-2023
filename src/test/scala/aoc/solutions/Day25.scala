package aoc.solutions
import aoc.AocTest
import zio.parser.*
import aoc.graph.*
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

class Day25 extends AocTest:
  // a machine a graph of nodes and edges where every node is connected to every other node
  case class Machine(edges: Set[Edge[String]]):
    val nodes = edges.flatMap(e => Set(e.a, e.b))

    def machineProduct: Int =
      val start = nodes.head

      val (maxFlow, residual) =
        (nodes - start).view
          .map(edmondsKarp(edges, start, _))
          .find(_._1 == 3)
          .get

      val graphTraversal = new GraphTraversal[String] with UnitNeighbors[String]:
        override val startNode: String = start

        override def unitNeighbors(node: String): IterableOnce[String] =
          for
            (toNode, r) <- residual(node)
            if r > 0
          yield toNode

      val component = BFS.traverse(graphTraversal).nodes
      val a         = component.size
      val b         = nodes.size - a
      a * b
    end machineProduct

  end Machine
  object Machine:
    def apply(s: String): Machine =
      val edges = s
        .split("\n")
        .flatMap { line =>
          val ss     = line.split(": ")
          val source = ss(0)
          val others = ss(1).split(" ")
          others.map { other =>
            Edge(source, other)
          }.toSet
        }
        .toSet
      Machine(edges)
    end apply
  end Machine

  val example = Machine(
    """jqt: rhn xhk nvd
      |rsh: frs pzl lsr
      |xhk: hfx
      |cmg: qnr nvd lhk bvb
      |rhn: xhk bvb hfx
      |bvb: xhk hfx
      |pzl: lsr hfx nvd
      |qnr: nvd
      |ntq: jqt hfx bvb xhk
      |nvd: lhk
      |lsr: lhk
      |rzs: qnr cmg lsr rsh
      |frs: qnr lhk lsr""".stripMargin
  )

  val machine = Machine(inputString)

  test("part 1"):
    assertEquals(example.machineProduct, 54)
    assertEquals(machine.machineProduct, 619225)
end Day25
