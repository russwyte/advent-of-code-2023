package aoc2023

class Day9 extends AocTest:
  val example =
    """0 3 6 9 12 15
    |1 3 6 10 15 21
    |10 13 16 21 30 45""".stripMargin
  def parse(s: String): Seq[Seq[Int]]     = s.split("\n").map(_.split(" ").map(_.toInt).toSeq).toSeq
  def differences(is: Seq[Int]): Seq[Int] = is.sliding(2).map(is => is(1) - is(0)).toSeq
  def reduceToZeros(is: Seq[Int]): Seq[Seq[Int]] =
    def inner(acc: List[Seq[Int]]): List[Seq[Int]] =
      if acc.head.forall(_ == 0) then acc
      else inner(differences(acc.head) :: acc)
    inner(List(is))
  def nextValue(is: Seq[Int]): Int     = reduceToZeros(is).foldLeft(0) { (acc, a) => acc + a.last }
  def previousValue(is: Seq[Int]): Int = reduceToZeros(is).foldLeft(0) { (acc, a) => a.head - acc }
  test("part1"):
    assertEquals(parse(example).map(nextValue).sum, 114)
    assertEquals(parse(input.mkString("\n")).map(nextValue).sum, 1955513104)
  test("part2"):
    assertEquals(parse(example).map(previousValue).sum, 2)
    assertEquals(parse(input.mkString("\n")).map(previousValue).sum, 1131)
end Day9
