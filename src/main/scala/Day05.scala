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

  private def countPoints(points: Seq[Point]): Int = {
    points.groupMapReduce(identity)(_ => 1)(_ + _).values.count(_ >= 2)
  }

  def part1(input: Seq[Line]): Int = {

    def lineToPoints(l: Line): Seq[Point] = {
      l match {
        case Line(Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
          val dx = if (x1 < x2) 1 else -1
          (x1 to x2 by dx).map(x => Point(x, y1))
        case Line(Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
          val dy = if (y1 < y2) 1 else -1
          (y1 to y2 by dy).map(y => Point(x1, y))
        case _ => Seq()
      }
    }

    val points = input.flatMap(lineToPoints)
    countPoints(points)
  }

  def part2(input: Seq[Line]): Int = {

    def lineToPoints(l: Line): Seq[Point] = {
      l match {
        case Line(Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
          val dx = if (x1 < x2) 1 else -1
          (x1 to x2 by dx).map(x => Point(x, y1))
        case Line(Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
          val dy = if (y1 < y2) 1 else -1
          (y1 to y2 by dy).map(y => Point(x1, y))
        case Line(Point(x1, y1), Point(x2, y2)) =>
          val dx = if (x1 < x2) 1 else -1
          val dy = if (y1 < y2) 1 else -1
          (x1 to x2 by dx).zip(y1 to y2 by dy).map { case (x, y) =>
            Point(x, y)
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
