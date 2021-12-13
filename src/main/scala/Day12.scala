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
          input
            .filter(n =>
              n.from == cur && (!isSmallCave(n.to) || !path.contains(n.to))
            )
            .map(_.to)
        next.map(n => dfs(input, path :+ cur, n)).sum
      }
    }

    dfs(input, Seq.empty, "start")
  }

  def part2(input: Seq[Node]): Int = {

    def dfs(
        input: Seq[Node],
        path: Seq[String],
        cur: String,
        counts: Map[String, Int]
    ): Int = {
      if (cur == "end") {
        1
      } else if (cur == "start" && counts.contains("start")) {
        0
      } else {
        val next = input
          .filter { n =>
            val timesVisited = counts.getOrElse(n.to, 0)
            n.from == cur && timesVisited <= 1
          }
          .map(_.to)

        val newCounts = if (isSmallCave(cur)) {
          counts.updated(cur, counts.getOrElse(cur, 0) + 1)
        } else counts

        if (newCounts.values.count(_ == 2) > 1) {
          0
        } else {
          next.map(n => dfs(input, path :+ cur, n, newCounts)).sum
        }
      }
    }

    dfs(input, Seq.empty, "start", Map.empty)
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
