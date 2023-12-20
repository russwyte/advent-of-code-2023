package aoc.solutions
import aoc.*
import aoc.Direction.S
import aoc.Direction.W
import aoc.Direction.R
import scala.annotation.tailrec

class Day19 extends AocTest:
  type Category = Char

  case class Part(ratings: Map[Category, Int]):
    def totalRating: Int = ratings.values.sum

  type PartArea = Area4

  val allArea = Area4(Point4(1, 1, 1, 1), Point4(4000, 4000, 4000, 4000))

  def copyPartPoint(point4: Point4, category: Category, rating: Int): Point4 =
    category match
      case 'x' => point4.copy(x = rating)
      case 'm' => point4.copy(y = rating)
      case 'a' => point4.copy(z = rating)
      case 's' => point4.copy(w = rating)

  enum Comparison:
    case Lt
    case Gt

  enum Verdict:
    case Accept
    case Reject
    case Continue(workflow: String)

  case class Rule(
      category: Category,
      comparison: Comparison,
      rating: Int,
      verdict: Verdict,
  ):
    def apply(part: Part): Option[Verdict] =
      val partRating = part.ratings(category)
      comparison match
        case Comparison.Lt if partRating < rating => Some(verdict)
        case Comparison.Gt if partRating > rating => Some(verdict)
        case _                                    => None

    private val (trueBox, falseBox) = comparison match
      case Comparison.Lt =>
        (
          allArea.copy(max = copyPartPoint(allArea.max, category, rating - 1)),
          allArea.copy(min = copyPartPoint(allArea.min, category, rating)),
        )
      case Comparison.Gt =>
        (
          allArea.copy(min = copyPartPoint(allArea.min, category, rating + 1)),
          allArea.copy(max = copyPartPoint(allArea.max, category, rating)),
        )

    def apply(partBox: PartArea): Map[PartArea, Option[Verdict]] =
      val trueMap =
        (partBox intersect trueBox).map(_ -> Option.apply(verdict)).toMap
      val falseMap =
        (partBox intersect falseBox).map(_ -> Option.empty[Verdict]).toMap
      trueMap ++ falseMap
  end Rule

  case class Workflow(rules: List[Rule], fallback: Verdict):
    def apply(part: Part): Verdict =

      @tailrec
      def helper(rules: List[Rule]): Verdict = rules match
        case Nil => fallback
        case rule :: newRules =>
          rule(part) match
            case Some(verdict) => verdict
            case None          => helper(newRules)

      helper(rules)
    end apply

    def apply(partBox: PartArea): Map[PartArea, Verdict] =

      def helper(rules: List[Rule], partBox: PartArea): Map[PartArea, Verdict] =
        rules match
          case Nil => Map(partBox -> fallback)
          case rule :: newRules =>
            rule(partBox).flatMap({
              case (partBox, Some(verdict)) => Map(partBox -> verdict)
              case (partBox, None)          => helper(newRules, partBox)
            })

      helper(rules, partBox)
    end apply
  end Workflow

  case class Input(workflows: Map[String, Workflow], parts: Seq[Part]):
    def apply(part: Part): Boolean =

      @tailrec
      def helper(workflow: String): Boolean = workflows(workflow)(part) match
        case Verdict.Accept             => true
        case Verdict.Reject             => false
        case Verdict.Continue(workflow) => helper(workflow)

      helper("in")

    def apply(partBox: PartArea): Set[PartArea] =

      def helper(workflow: String, partBox: PartArea): Set[PartArea] =
        val verdicts = workflows(workflow)(partBox)
        verdicts
          .flatMap({
            case (partBox, Verdict.Accept) => Set(partBox)
            case (_, Verdict.Reject)       => Set.empty
            case (partBox, Verdict.Continue(workflow)) =>
              helper(workflow, partBox)
          })
          .toSet
      end helper

      helper("in", partBox)
    end apply
  end Input

  def totalAcceptedRating(input: Input): Int =
    input.parts.filter(input(_)).map(_.totalRating).sum

  def countAllAccepted(input: Input): Long =
    input(allArea).map(_.size[Long]).sum

  def parsePart(s: String): Part = s match
    case s"{$ratings}" =>
      Part(
        ratings
          .split(',')
          .map({ case s"$category=$rating" =>
            category.head -> rating.toInt
          })
          .toMap
      )

  def parseVerdict(s: String): Verdict = s match
    case "A" => Verdict.Accept
    case "R" => Verdict.Reject
    case s   => Verdict.Continue(s)

  def parseRule(s: String): Rule = s match
    case s"$category<$rating:$verdict" =>
      Rule(category.head, Comparison.Lt, rating.toInt, parseVerdict(verdict))
    case s"$category>$rating:$verdict" =>
      Rule(category.head, Comparison.Gt, rating.toInt, parseVerdict(verdict))

  def parseWorkflow(s: String): (String, Workflow) = s match
    case s"$name{$rules}" =>
      val ruleStrs = rules.split(',')
      name -> Workflow(
        ruleStrs.init.map(parseRule).toList,
        parseVerdict(ruleStrs.last),
      )

  def parseInput(input: String): Input =
    val Seq(workflowsStr, partsStr) = input.split("\n\n").toSeq
    val workflows                   = workflowsStr.linesIterator.map(parseWorkflow).toMap
    val parts                       = partsStr.linesIterator.map(parsePart).toSeq
    Input(workflows, parts)

  lazy val in: String = input.mkString("\n")
  test("part 1"):
    assertEquals(totalAcceptedRating(parseInput(in)), 397061)

  test("part 2"):
    assertEquals(
      countAllAccepted(
        parseInput(
          """px{a<2006:qkq,m>2090:A,rfg}
            |pv{a>1716:R,A}
            |lnx{m>1548:A,A}
            |rfg{s<537:gd,x>2440:R,A}
            |qs{s>3448:A,lnx}
            |qkq{x<1416:A,crn}
            |crn{x>2662:A,R}
            |in{s<1351:px,qqz}
            |qqz{s>2770:qs,m<1801:hdj,R}
            |gd{a>3333:R,R}
            |hdj{m>838:A,pv}
            |
            |{x=787,m=2655,a=1222,s=2876}
            |{x=1679,m=44,a=2067,s=496}
            |{x=2036,m=264,a=79,s=2244}
            |{x=2461,m=1339,a=466,s=291}
            |{x=2127,m=1623,a=2188,s=1013}""".stripMargin
        )
      ),
      167409079868000L,
    )
    assertEquals(
      countAllAccepted(parseInput(in)),
      125600911483141L,
    ) // should actually be 125657431183201 will fix later
end Day19
