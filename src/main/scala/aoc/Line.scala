package aoc

case class Line(start: Point, end: Point):
  def length: Int           = start.manhattanDistance(end)
  def dx                    = end.x - start.x
  def dy                    = end.y - start.y
  def slope: Double         = dy.toDouble / dx.toDouble
  def yIntercept: Double    = start.y - slope * start.x
  def isVertical: Boolean   = dx == 0
  def isHorizontal: Boolean = dy == 0
  def xRange: Range = start.x to end.x by (if isHorizontal then
                                             val x = dx.sign
                                             if x == 0 then 1 else x
                                           else 1)
  def yRange: Range = start.y to end.y by (if isVertical then
                                             val y = dy.sign
                                             if y == 0 then 1 else y
                                           else 1)
  def points =
    if isVertical then yRange.map(y => Point(start.x, y))
    else if isHorizontal then xRange.map(x => Point(x, start.y))
    else xRange.map(x => Point(x, (slope * x + yIntercept).round.toInt))
end Line
