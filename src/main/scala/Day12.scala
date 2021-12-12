import scala.io.Source

case class Node(from: String, to: String)

object Day12 extends App {

  def readFile(filename: String): Seq[Node] =
    Source
      .fromResource(filename)
      .getLines
      .flatMap { l =>
        val fromTo = l.split("-")
        Seq(Node(fromTo(0), fromTo(1)), Node(fromTo(1), fromTo(0)))
      }
      .toSeq

  private def isSmallCave(cave: String): Boolean = cave.toLowerCase == cave

  def part1(input: Seq[Node]): Int = {

    def dfs(input: Seq[Node], path: Seq[String], cur: String): Int = {
      if (cur == "end") {
        1
      } else {
        val next =
          input.filter(n => n.from == cur && (!isSmallCave(n.to) || !path.contains(n.to))).map(_.to)
        next.map(n => dfs(input, path :+ cur, n)).sum
      }
    }

    dfs(input, Seq.empty, "start")
  }

  def part2(input: Seq[Node]): Int = {

    def countVisits(path: Seq[String]): Map[String, Int] =
      path.filter(isSmallCave).groupMapReduce(identity)(_ => 1)(_ + _)

    def dfs(input: Seq[Node], path: Seq[String], cur: String): Int = {
      if (cur == "end") {
        val visitedTwice = countVisits(path).values.count(_ == 2)
        if (visitedTwice == 1 || visitedTwice == 0) 1 else 0
      } else if (cur == "start" && path.contains("start")) {
        0
      } else {
        val counts = countVisits(path)
        if (counts.values.count(_ == 2) > 1) {
          return 0
        }

        val next = input
          .filter { n =>
            val timesVisited = counts.getOrElse(n.to, 0)
            n.from == cur && (timesVisited == 1 || timesVisited == 0)
          }
          .map(_.to)

        next.map(n => dfs(input, path :+ cur, n)).sum
      }
    }

    dfs(input, Seq.empty, "start")
  }

  val sample1 = readFile("sample_12_1")
  val sample = readFile("sample_12")
  assert(part1(sample1) == 10)
  assert(part1(sample) == 226)

  val items = readFile("input_12")
  println(s"Part 1: ${part1(items)}")

  assert(part2(sample1) == 36)
  assert(part2(sample) == 3509)

  println(s"Part 2: ${part2(items)}")
}
