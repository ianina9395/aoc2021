import scala.io.Source

object Day07 extends App {

  def readFile(filename: String): Seq[Int] =
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .head
      .split(",")
      .map(_.toInt)
      .toSeq

  def part1(input: Seq[Int]): Int = {
    (input.min to input.max).map { i =>
      input.map(n => Math.abs(n - i)).sum
    }.min
  }

  def part2(input: Seq[Int]): Int = {

    def weightedDiff(n1: Int, n2: Int): Int = {
      val diff = Math.abs(n1 - n2)
      (1 to diff).sum
    }

    (input.min to input.max).map { i =>
      input.map(n => weightedDiff(n, i)).sum
    }.min
  }

  val sample = Seq(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
  assert(part1(sample) == 37)
  assert(part2(sample) == 168)

  val items: Seq[Int] = readFile("input_07")
  println(s"Part 1: ${part1(items)}")
  println(s"Part 2: ${part2(items)}")

}
