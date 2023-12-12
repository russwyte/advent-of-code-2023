package aoc2023

import scala.io.Source
import zio.parser.Syntax
import zio.Chunk

abstract class AocTest extends munit.FunSuite:
  self =>
  val input: Vector[String] = Source.fromResource(self.getClass.getSimpleName + ".txt").getLines().toVector
object AocTest:
  val intSyntax: Syntax[String, Char, Char, Int] = Syntax.digit.repeat.transform(
    { case chars: Chunk[Char] => chars.mkString.toInt },
    { case i: Int => Chunk.fromIterable(i.toString) },
  )
  val longSyntax: Syntax[String, Char, Char, Long] = Syntax.digit.repeat.transform(
    { case chars: Chunk[Char] => chars.mkString.toLong },
    { case i: Long => Chunk.fromIterable(i.toString) },
  )
  val whiteSpaceSep = Syntax.whitespace.+.transform(_ => (), _ => Chunk.empty)
end AocTest
