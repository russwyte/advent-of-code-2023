package aoc.solutions

import scala.annotation.tailrec
import aoc.*

class Day8 extends AocTest:
  import Day8.*
  val example1 =
    """RL
    |
    |AAA = (BBB, CCC)
    |BBB = (DDD, EEE)
    |CCC = (ZZZ, GGG)
    |DDD = (DDD, DDD)
    |EEE = (EEE, EEE)
    |GGG = (GGG, GGG)
    |ZZZ = (ZZZ, ZZZ)""".stripMargin
  val example2 =
    """LLR
    |
    |AAA = (BBB, BBB)
    |BBB = (AAA, ZZZ)
    |ZZZ = (ZZZ, ZZZ)""".stripMargin
  val example3 =
    """LR
    |
    |11A = (11B, XXX)
    |11B = (XXX, 11Z)
    |11Z = (11B, XXX)
    |22A = (22B, XXX)
    |22B = (22C, 22C)
    |22C = (22Z, 22Z)
    |22Z = (22B, 22B)
    |XXX = (XXX, XXX)""".stripMargin

  val example1Network = networkSyntax.parseString(example1).toOption.get
  val example2Network = networkSyntax.parseString(example2).toOption.get
  val example3Network = networkSyntax.parseString(example3).toOption.get
  val network         = networkSyntax.parseString(input.mkString("\n")).toOption.get

  test("part1"):
    assertEquals(example1Network.countStepsToEnd("AAA", _ == "ZZZ"), BigInt(2))
    assertEquals(example2Network.countStepsToEnd("AAA", _ == "ZZZ"), BigInt(6))
    assertEquals(network.countStepsToEnd("AAA", _ == "ZZZ"), BigInt(16897))

  test("part2"):
    assertEquals(example3Network.countStepsSynchronizeAllStarts, BigInt(6))
    assertEquals(network.countStepsSynchronizeAllStarts, BigInt(16563603485021L))
end Day8

object Day8:
  import zio.parser.*
  import zio.Chunk

  enum Step(c: Char):
    def char = c
    case Left  extends Step('L')
    case Right extends Step('R')
  object Step:
    def apply(c: Char): Step = c match
      case 'L' => Left
      case 'R' => Right

  case class Node(name: String, left: String, right: String):
    def apply(step: Step): String = step match
      case Step.Left  => left
      case Step.Right => right

  case class Network(steps: List[Step], nodes: List[Node]):
    val nodeMap = nodes.map(n => n.name -> n).toMap

    def countStepsSynchronizeAllStarts: BigInt =
      nodes.filter(_.name.endsWith("A")).map(node => countStepsToEnd(node.name, _ endsWith "Z")).reduce(_ lcm _)

    def countStepsToEnd(startName: String, isEnd: String => Boolean): BigInt =
      @tailrec
      def inner(remainingSteps: List[Step], node: Node, count: Int): Int =
        if isEnd(node.name) then count
        else
          remainingSteps match
            case Nil          => inner(steps, node, count)
            case step :: rest => inner(rest, nodeMap(node(step)), count + 1)
      inner(steps, nodeMap(startName), 0)
    end countStepsToEnd
  end Network

  extension (a: BigInt) def lcm(b: BigInt): BigInt = a * b / a.gcd(b)

  val nameSyntax = Syntax.alphaNumeric.repeat.toList.transform(_.mkString, _.toList)
  val nodeSyntax =
    (nameSyntax ~ Syntax.string(" = (", ()) ~ nameSyntax ~ Syntax.string(", ", ()) ~ nameSyntax ~ Syntax
      .char(')'))
      .transform(
        Node.apply.tupled,
        node => (node.name, node.left, node.right),
      )
  val stepsSyntax =
    Syntax.charIn("LR").repeat.toList.transform(_.map(Step.apply), _.map(_.char)) ~ Syntax.char('\n') ~ Syntax.char(
      '\n'
    )
  val networkSyntax =
    (stepsSyntax ~ nodeSyntax
      .repeatWithSep(Syntax.char('\n'))
      .toList).transform(
      Network.apply.tupled,
      network => (network.steps, network.nodes),
    )
end Day8
