package aoc.solutions
import aoc.*
import scala.annotation.tailrec
import scala.concurrent.duration.*

class Day22 extends AocTest:
  override val munitTimeout = Duration(120, "s")
  // A brick is a rectangular cuboid, specified by two opposite corners.
  // the Z coordinate is the height of the brick with respect to the floor so it is always positive with 1 being the lowest possible value
  case class Brick(name: String, a: Area3):
    def fall: Brick =
      copy(a = a.copy(min = a.min.copy(z = a.min.z - 1), max = a.max.copy(z = a.max.z - 1)))
  object Brick:
    def apply(s: String): Brick =
      s match
        case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
          Brick(s, Area3(Point3(x1.toInt, y1.toInt, z1.toInt), Point3(x2.toInt, y2.toInt, z2.toInt)))

  // A pile is a collection of bricks.
  case class Pile(bricks: Brick*):
    val MinZ = 1
    // let's write a to settle the pile such that bricks are stacked on top of each other
    // and the pile is as short as possible - bricks will not change their orientation
    // to accomplish this we should move each brick starting from the bottom until it hits another brick or rests on the floor
    def settle: Pile =
      @tailrec
      def settleBrick(brick: Brick, settled: Vector[Brick]): Brick =
        if brick.a.min.z == MinZ then brick
        else
          val newBrick = brick.fall
          settled.forall { s =>
            val res = s.a.intersect(newBrick.a).isEmpty
            res
          } match
            case true =>
              settleBrick(newBrick, settled)
            case false =>
              brick
      val settledBricks = bricks.foldLeft(Vector.empty[Brick]) { (acc, brick) =>
        acc.appended(settleBrick(brick, acc))
      }
      val res = Pile(settledBricks*)
      if res.isStable then res
      else res.settle
    end settle
    // let write a function to check if a pile is stable
    // a pile is stable if all bricks all resting on the floor or on other bricks
    def isStable: Boolean =
      bricks.forall { brick =>
        brick.a.min.z == MinZ || {
          val rest = bricks.filterNot(_ == brick)
          rest.find(b => b.a.intersect(brick.fall.a).isDefined).isDefined
        }
      }

    // let's write a function to check if a brick is safe
    // a brick is safe if it can be removed and the pile will remain stable
    def isBrickSafe(brick: Brick): Boolean =
      val newPile = Pile(bricks.filterNot(_ == brick)*)
      newPile.isStable

    def safeBricks = bricks.filter(isBrickSafe)

    // let's write a function to count the number of bricks that would fall if we remove a brick
    def countFallingBricks(brick: Brick): Int =
      val set     = bricks.toSet - brick
      val newPile = Pile(bricks.filterNot(_ == brick)*).settle
      val newSet  = newPile.bricks.toSet
      val diff    = set.diff(newSet)
      diff.size
    end countFallingBricks

    def part2 =
      val settled = settle
      settled.bricks.reverse.map { settled.countFallingBricks(_) }.sum

  end Pile
  object Pile:
    def apply(s: String): Pile =
      Pile(s.split("\n").map(Brick(_)).sortBy(b => (b.a.min.z, b.a.min.x, b.a.min.y))*)
  val pile = Pile(inputString).settle
  val example = Pile("""1,0,1~1,2,1
    |0,0,2~2,0,2
    |0,2,3~2,2,3
    |0,0,4~0,2,4
    |2,0,5~2,2,5
    |0,1,6~2,1,6
    |1,1,8~1,1,9""".stripMargin).settle

  // both solutions are way too slow - will fix later
  test("part 1"):
    assertEquals(example.safeBricks.size, 5)
    assertEquals(pile.safeBricks.size, 395)
  test("part 2"):
    assertEquals(example.part2, 7)
    assertEquals(pile.part2, 64714)

end Day22
