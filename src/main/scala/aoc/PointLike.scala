package aoc

trait PointLike[A <: PointLike[A]]:
  def +(that: A): A
  def -(that: A): A
  def *(n: Int): A
  def offset(that: A, count: Int): A
  def manhattanDistance(that: A): Int
  def min(that: A): A
  def max(that: A): A
  def <=(that: A): Boolean
end PointLike
