package aoc

import scala.io.Source
import zio.parser.*
import zio.Chunk

abstract class AocTest extends munit.FunSuite:
  self =>
  def inputIterator: Iterator[String] = Source.fromResource(self.getClass.getSimpleName + ".txt").getLines()
  def inputString: String             = inputIterator.mkString("\n")
  def input: Vector[String]           = inputIterator.toVector

object AocTest:
  val numStrSyntax: Syntax[String, Char, Char, String] =
    (Syntax.charIn("-").? ~ Syntax.digit.repeat.toList.transform(_.mkString, _.toList)).transform(
      { case (sign, digits) =>
        sign.getOrElse("").toString() + digits
      },
      { case s: String =>
        if s.startsWith("-") then (Some('-'), s.substring(1))
        else (None, s)
      },
    )
  val intSyntax  = numStrSyntax.transform(_.toInt, _.toString)
  val longSyntax = numStrSyntax.transform(_.toLong, _.toString)

  val whiteSpaceSep = Syntax.whitespace.+.transform(_ => (), _ => Chunk.empty)
  val dirSyntax = Syntax
    .charIn("UDLR")
    .transform(
      {
        case 'U' => Direction.Up
        case 'D' => Direction.Down
        case 'L' => Direction.Left
        case 'R' => Direction.Right
      },
      {
        case Direction.Up    => 'U'
        case Direction.Down  => 'D'
        case Direction.Left  => 'L'
        case Direction.Right => 'R'
        case _               => ??? // impossible
      },
    )
end AocTest
