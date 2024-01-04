package aoc.solutions
import aoc.geo.*
import zio.parser.*
import aoc.AocTest.*
import aoc.Point

class Day24 extends aoc.AocTest:
  extension [A](self: Iterator[A]) def findMap[B](f: A => Option[B]): B = self.flatMap(f).next()

  case class Hail2D(position: Point2[Long], velocity: Point2[Long]):
    def timeTo(p: Point2[Long]): Double =
      if velocity.x != 0 then (p.x - position.x) / velocity.x
      else if velocity.y != 0 then (p.y - position.y) / velocity.y
      else Double.MaxValue
    def next: Hail2D = copy(position = position + velocity)
    def line         = Line2(position, next.position)
    def willIntersect(other: Hail2D): Option[Intersection2D] =
      line.intersect(other.line).map { p => Intersection2D(this, other, p, timeTo(p), other.timeTo(p)) }
    def deltaVelocity(v: Point2[Long]): Hail2D = copy(velocity = velocity - v)
  end Hail2D
  case class Hail(position: Point3[Long], velocity: Point3[Long]):
    import Hail.*
    override def toString(): String = s"(${syntax.printString(this).toOption.getOrElse("")})"
    def xy                          = Hail2D(position.xy, velocity.xy)
    def xz                          = Hail2D(position.xz, velocity.xz)
    def pathsIntersectInXandY(other: Hail)(using r: DoubleRounder[Long]): Option[Intersection] =
      xy.willIntersect(other.xy).flatMap { i2d =>
        val z1 = other.position.z + i2d.t1 * other.velocity.z
        val z2 = other.position.z + i2d.t2 * other.velocity.z
        val z  = (z1 + z2) / 2
        Some(Intersection(this, other, Point3(i2d.p.x, i2d.p.y, r(z)), i2d.t1, i2d.t2))
      }
    end pathsIntersectInXandY
  end Hail
  object Hail:
    val c      = Syntax.char(',') ~ whiteSpaceSep
    val middle = whiteSpaceSep ~ Syntax.char('@') ~ whiteSpaceSep
    val syntax =
      (longSyntax ~ c ~ longSyntax ~ c ~ longSyntax ~ middle ~ longSyntax ~ c ~ longSyntax ~ c ~ longSyntax)
        .transform(
          { case (x, y, z, vx, vy, vz) =>
            Hail(Point3(x, y, z), Point3(vx, vy, vz))
          },
          { case Hail(Point3(x, y, z), Point3(vx, vy, vz)) =>
            (x, y, z, vx, vy, vz)
          },
        )
    def apply(s: String): Hail = syntax.parseString(s).toOption.get
  end Hail
  case class Intersection2D(a: Hail2D, b: Hail2D, p: Point2[Long], t1: Double, t2: Double)
  case class Intersection(a: Hail, b: Hail, p: Point3[Long], t1: Double, t2: Double)

  case class Spiral(p: Point2[Long], v: Point2[Long], count: Long, limit: Long):
    def next: Spiral =
      if count > 0 then copy(p = p + v, count = count - 1)
      else if v.y == 0 then copy(p = p + v, v = v.copy(y = v.x, x = -v.y), count = limit)
      else copy(p = p + v, v = v.copy(x = -v.y, y = v.x), count = limit + 1, limit = limit + 1)
  end Spiral
  object Spiral:
    val Start = Spiral(Point2(0L, 0L), Point2(1L, 0L), 0, 0)

  case class Storm(hailStones: List[Hail], areaXY: Area2[Long]):
    def willIntersect: Set[Intersection] =
      hailStones
        .combinations(2)
        .flatMap { l => l(0).pathsIntersectInXandY(l(1)) }
        .filter(x => x.t1 >= 0 && x.t2 >= 0 && areaXY.contains(x.p.xy))
        .toSet
    end willIntersect

    def findRockOrigin(hails: Seq[Hail2D], v: Point2[Long]): Option[Point2[Long]] =
      val hs = hails.map(_.deltaVelocity(v))
      hs match
        case h0 :: h1 :: h2 :: _ =>
          for
            i1 <- h0.willIntersect(h1)
            i2 <- h0.willIntersect(h2)
            if i1.p == i2.p
            time = i2.t1.toLong
          yield h0.position + (h0.velocity * time)
        case _ => None
    end findRockOrigin

    def sumCoordinatesForRockOrigin: Long =
      val xys = hailStones.map(_.xy)
      val xzs = hailStones.map(_.xz)
      val xy = Iterator.iterate(Spiral.Start)(_.next).findMap { s =>
        findRockOrigin(xys, s.p)
      }
      val xz = Iterator.iterate(Spiral.Start)(_.next).findMap { s =>
        findRockOrigin(xzs, s.p)
      }
      xy.x + xy.y + xz.y // xz.y here is actually the z coordinate
    end sumCoordinatesForRockOrigin

  end Storm
  object Storm:
    def apply(s: String, areaXY: Area2[Long]): Storm = Storm(s.split("\n").map(Hail(_)).toList, areaXY)

  val example = Storm(
    """19, 13, 30 @ -2,  1, -2
      |18, 19, 22 @ -1, -1, -2
      |20, 25, 34 @ -2, -2, -4
      |12, 31, 28 @ -1, -2, -1
      |20, 19, 15 @  1, -5, -3""".stripMargin,
    Area2(7L, 27L, 7L, 27L),
  )
  val storm = Storm(inputString, Area2(200000000000000L, 400000000000000L, 200000000000000L, 400000000000000L))
  test("part 1"):
    assertEquals(example.willIntersect.size, 2)
    assertEquals(storm.willIntersect.size, 27732)

  test("part 2"):
    assertEquals(example.sumCoordinatesForRockOrigin, 47L)
    assertEquals(storm.sumCoordinatesForRockOrigin, 641619849766168L)
end Day24
