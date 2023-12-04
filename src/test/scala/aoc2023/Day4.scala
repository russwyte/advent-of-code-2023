package aoc2023

import javax.swing.CellEditor
import java.awt.GridLayout

class Day4 extends AocTest:
  import Day4.*
  val example =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin

  val exampleCards = example.split("\n").flatMap(cardSyntax.parseString(_).toSeq)
  val inputCards   = input.flatMap(cardSyntax.parseString(_).toSeq)

  test("part1"):
    assertEquals(exampleCards.map(_.value).sum, 13)
    assertEquals(inputCards.map(_.value).sum, 21105)

  test("part2"):
    val exampleGame = Game(exampleCards)
    val inputGame   = Game(inputCards)
    assertEquals(exampleGame.play, 30)
    assertEquals(inputGame.play, 5329815)
end Day4
object Day4:
  import AocTest.*
  import zio.parser.*
  import zio.Chunk

  case class Card(n: Int, winningNumbers: Seq[Int], hadNumbers: Seq[Int]):
    val value   = hadNumbers.filter(winningNumbers.contains).foldLeft(0)((acc, _) => if acc == 0 then 1 else acc * 2)
    val matches = hadNumbers.count(winningNumbers.contains)

  case class Game(cards: Seq[Card]):
    def play: Int =
      val a = Array.fill(cards.length)(1)
      cards.foreach { card =>
        val idx = card.n - 1
        (idx + 1 until Math.min(cards.length, idx + 1 + card.matches)).foreach { i =>
          a.update(i, a(i) + 1 * a(idx))
        }
      }
      a.sum
    end play
  end Game

  val numbersSyntax = intSyntax.repeatWithSep(whiteSpaceSep)

  val cardSyntax =
    (Syntax.string("Card", ()) ~
      whiteSpaceSep ~
      intSyntax ~
      Syntax.string(":", ()) ~
      whiteSpaceSep ~
      numbersSyntax ~
      Syntax.string(" |", ()) ~
      whiteSpaceSep ~
      numbersSyntax).transform(
      { case (n, winningNumbers, hadNumbers) =>
        Card(n, winningNumbers, hadNumbers)
      },
      { case Card(n, winningNumbers, hadNumbers) =>
        (n, Chunk.fromIterable(winningNumbers), Chunk.fromIterable(hadNumbers))
      },
    )
end Day4
