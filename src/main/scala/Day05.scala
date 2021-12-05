import scala.collection.mutable
import scala.io.Source

case class Point(x: Int, y: Int)
case class Line(start: Point, end: Point)

object Day05 extends App {

  def readFile(filename: String): Seq[Line] = {
    val lineRegex = """(\d+),(\d+) -> (\d+),(\d+)""".r
    Source
      .fromResource(filename)
      .getLines
      .map { case lineRegex(x1, y1, x2, y2) =>
        Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      }
      .toSeq
  }

  private def countPoints(points: Seq[Point]) = {
    val counts = mutable.Map[Point, Int]()
    points.foreach { p =>
      counts += p -> (counts.getOrElse(p, 0) + 1)
    }
    counts.values.count(_ >= 2)
  }

  def part1(input: Seq[Line]): Int = {

    def lineToPoints(l: Line): Seq[Point] = {
      if (l.start.y == l.end.y) {
        val dx = if (l.start.x < l.end.x) 1 else -1
        (l.start.x to l.end.x by dx).map(x => Point(x, l.start.y))
      } else if (l.start.x == l.end.x) {
        val dy = if (l.start.y < l.end.y) 1 else -1
        (l.start.y to l.end.y by dy).map(y => Point(l.start.x, y))
      } else Seq()
    }

    val points = input.flatMap(lineToPoints)
    countPoints(points)
  }

  def part2(input: Seq[Line]): Int = {

    def lineToPoints(l: Line): Seq[Point] = {
      if (l.start.y == l.end.y) {
        val dx = if (l.start.x < l.end.x) 1 else -1
        (l.start.x to l.end.x by dx).map(x => Point(x, l.start.y))
      } else if (l.start.x == l.end.x) {
        val dy = if (l.start.y < l.end.y) 1 else -1
        (l.start.y to l.end.y by dy).map(y => Point(l.start.x, y))
      } else {
        val dx = if (l.start.x < l.end.x) 1 else -1
        val dy = if (l.start.y < l.end.y) 1 else -1
        (l.start.x to l.end.x by dx).zip(l.start.y to l.end.y by dy).map {
          case (x, y) => Point(x, y)
        }
      }
    }

    val points = input.flatMap(lineToPoints)
    countPoints(points)
  }

  val sample = readFile("sample_05")
  assert(part1(sample) == 5)
  assert(part2(sample) == 12)

  val items: Seq[Line] = readFile("input_05")
  println(s"Part 1: ${part1(items)}")
  println(s"Part 2: ${part2(items)}")

}
