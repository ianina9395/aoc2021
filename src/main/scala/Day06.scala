import scala.collection.mutable
import scala.io.Source

object Day06 extends App {

  def readFile(filename: String): Seq[Int] =
    Source
      .fromResource(filename)
      .getLines
      .toSeq
      .head
      .split(",")
      .map(_.toInt)
      .toSeq

  // naive
  def part1(input: Seq[Int], days: Int): Int = {
    var buf = input
    (0 until days).foreach { _ =>
      val newFish = Seq.fill(buf.count(_ == 0))(8)
      buf = buf.map(i => if (i == 0) 6 else i - 1) ++ newFish
    }
    buf.length
  }

  def part2(input: Seq[Int], days: Int): Long = {
    var counts: mutable.Map[Int, Long] = mutable.Map()
    counts ++= input.groupMapReduce(identity)(_ => 1L)(_ + _)

    (0 until days).foreach { _ =>
      counts = counts.map { case (num, c) =>
        (num - 1, c)
      }
      counts.remove(-1).map { newFish =>
        counts.put(6, counts.getOrElse(6, 0L) + newFish)
        counts.put(8, newFish)
      }
    }
    counts.view.values.sum
  }

  val sample = Seq(3, 4, 3, 1, 2)
  assert(part1(sample, 18) == 26)
  assert(part1(sample, 80) == 5934)

  assert(part2(sample, 18) == 26)
  assert(part2(sample, 80) == 5934)

  val items: Seq[Int] = readFile("input_06")
  println(s"Part 1: ${part1(items, 80)}")

  assert(part2(sample, 256) == 26984457539L)
  println(s"Part 2: ${part2(items, 256)}")

}
