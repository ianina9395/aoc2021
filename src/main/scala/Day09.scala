import scala.collection.mutable
import scala.io.Source

case class HeightMap(data: Seq[Seq[Int]]) {
  val rows = data.length
  val cols = data.head.length

  def adj(r: Int, c: Int): Seq[(Int, Int)] = {

    def checkValid(p: (Int, Int)): Boolean = {
      val (i, j) = p
      i <= (rows - 1) && i >= 0 && j <= (cols - 1) && j >= 0
    }

    Seq(
      (r, c - 1),
      (r, c + 1),
      (r - 1, c),
      (r + 1, c)
    ).filter(checkValid)
  }
}

object Day09 extends App {

  def readFile(filename: String): HeightMap = {
    val d = Source
      .fromResource(filename)
      .getLines
      .map(_.split("").map(_.toInt).toSeq)
      .toSeq
    println(d)
    HeightMap(d)
  }

  def findLowPoints(input: HeightMap): Seq[(Int, Int)] = {
    (for (i <- 0 until input.rows; j <- 0 until input.cols)
      yield (i, j)).filter { case (i, j) =>
      val adj = input.adj(i, j).map { case (i, j) => input.data(i)(j) }
      adj.count(adj => adj > input.data(i)(j)) == adj.length
    }
  }

  def part1(input: HeightMap): Int = {
    findLowPoints(input).map { case (i, j) =>
      input.data(i)(j) + 1
    }.sum
  }

  def part2(input: HeightMap) = {

    def findResin(center: (Int, Int)): Int = {
      val queue = mutable.Queue[(Int, Int)]()
      queue.enqueue(center)

      val resin = mutable.Set[(Int, Int)]()
      while (queue.nonEmpty) {
        val (i, j) = queue.dequeue()
        resin.add(i, j)
        val adj = input.adj(i, j).filter { case (i, j) =>
          input.data(i)(j) < 9
        }
        queue.enqueueAll(adj.filterNot { i => resin.contains(i) })
      }
      resin.size
    }

    findLowPoints(input).map(findResin).sorted.reverse.take(3).product
  }

  val sample = readFile("sample_09")
  println(part1(sample))
  assert(part1(sample) == 15)

  val items = readFile("input_09")
  println(s"Part 1: ${part1(items)}")

  assert(part2(sample) == 1134)
  println(s"Part 2: ${part2(items)}")

}
