package aoc.solutions

import aoc.*
import zio.parser.*
import scala.collection.immutable.ListMap

class Day15 extends AocTest:
  import AocTest.*
  // turns any sting into a hash value between 0 and 255 inclusive
  def hash(s: String): Int = s.foldLeft(0): (acc, c) =>
    ((acc + c) * 17) % 256
  val example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".split(",").toSeq
  val codes   = input.mkString.split(",").toSeq

  val nameSyntax = Syntax.alphaNumeric.repeat.toList.transform(_.mkString, _.toCharArray().toList)

  val operationSynax =
    (nameSyntax ~ Syntax.charIn("=-") ~ intSyntax.?).transform(
      {
        case (name, '=', Some(length)) => Operation.AddOrReplace(name, length)
        case (name, _, _)              => Operation.Remove(name)
      },
      {
        case Operation.AddOrReplace(name, length) => (name, '=', Some(length))
        case Operation.Remove(name)               => (name, '-', None)
      },
    )

  type Slots  = ListMap[String, Int]
  type BoxMap = Map[Int, Slots]

  enum Operation:
    def label: String
    case AddOrReplace(label: String, length: Int)
    case Remove(label: String)

  case class Step(hash: Int, operation: Operation)
  object Step:
    def apply(s: String): Step =
      val op = operationSynax.parseString(s).toOption.get
      Step(hash(op.label), op)

  def part2(ss: Seq[String]) =
    val steps = ss.map(Step(_))
    val boxMap: BoxMap = steps.foldLeft(Map.empty[Int, Slots]): (boxMap, step) =>
      step match
        case Step(hash, Operation.AddOrReplace(label, length)) =>
          val slots = boxMap.getOrElse(hash, ListMap.empty)
          boxMap.updated(hash, slots.updated(label, length))
        case Step(hash, Operation.Remove(label)) =>
          val slots = boxMap.get(hash).getOrElse(ListMap.empty)
          boxMap.updated(hash, slots - label)
    boxMap.keys.foldLeft(0) { (acc, hash) =>
      val slots    = boxMap(hash)
      val multiple = hash + 1
      acc + slots.zipWithIndex.foldLeft(0) { case (acc, ((label, length), idx)) =>
        val res = multiple * (idx + 1) * length
        acc + res
      }
    }
  end part2
  test("Part 1"):
    assertEquals(
      example.map(hash).sum,
      1320,
    )
    assertEquals(
      codes.map(hash).sum,
      494980,
    )
  test("Part 2"):
    assertEquals(
      part2(example),
      145,
    )
    assertEquals(
      part2(codes),
      247933,
    )
end Day15
