package aoc.solutions
import aoc.AocTest
import zio.parser.*
import aoc.graph.*
import scala.collection.immutable.Queue
import scala.collection.mutable

class Day25 extends AocTest:
  case class Machine(edges: Set[Edge[String]]):
    val nodes = edges.flatMap(e => Set(e.a, e.b))

    def size: Int = nodes.size

    def machineProduct: Option[Int] =
      (nodes - nodes.head).view
        .map(EdmondsKarp(edges, nodes.head, _))
        .find(_.maxFlow == 3)
        .map: r =>
          val graphTraversal = new GraphTraversal[String] with UnitNeighbors[String]:
            override val startNode: String = nodes.head

            override def unitNeighbors(node: String): IterableOnce[String] =
              r.residual(node).filter { case (_, r) => r > 0 }.map { case (n, _) => n }

          val d1 = BFS.traverse(graphTraversal).nodes
          val d2 = nodes -- d1
          val m1 = Machine(
            edges.filter(e => d2.contains(e.a) && d2.contains(e.b))
          )
          val m2 = Machine(
            edges.filter(e => d1.contains(e.a) && d1.contains(e.b))
          )
          m1.size * m2.size

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
    assertEquals(example.machineProduct, Some(54))
    assertEquals(machine.machineProduct, Some(619225))
end Day25
