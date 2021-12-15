import scala.collection.mutable
import scala.io.Source

object Day15 extends App {

  case class Pos(r: Int, c: Int)

  case class RiskMap(data: Seq[Seq[Int]]) {
    val rows = data.length
    val cols = data.head.length

    def adj(p: Pos): Seq[Pos] = {

      def checkValid(p: Pos): Boolean = {
        p.r <= (rows - 1) && p.r >= 0 && p.c <= (cols - 1) && p.c >= 0
      }

      Seq(
        Pos(p.r, p.c - 1),
        Pos(p.r, p.c + 1),
        Pos(p.r - 1, p.c),
        Pos(p.r + 1, p.c)
      ).filter(checkValid)
    }
  }

  def readFile(filename: String): RiskMap =
    RiskMap(
      Source
        .fromResource(filename)
        .getLines
        .map(_.split("").map(_.toInt).toSeq)
        .toSeq
    )

  def dijkstra(input: RiskMap): Int = {
    val start = Pos(0, 0)
    val end = Pos(input.rows - 1, input.cols - 1)

    val queue = mutable.PriorityQueue[(Pos, Int)]()(
      Ordering.by((_: (Pos, Int))._2).reverse
    )
    val weights = mutable.Map[Pos, Int]()
    val visited = mutable.Set[Pos]()

    queue.enqueue(start -> 0)
    weights += start -> 0
    for (r <- 0 until input.rows; c <- 0 until input.cols) {
      if (Pos(r, c) != start) {
        weights += Pos(r, c) -> Int.MaxValue
      }
    }

    while (queue.nonEmpty) {
      val (pos, w) = queue.dequeue()
      visited += pos

      if (pos == end) {
        return w
      }

      val adj = input.adj(pos).filter(!visited.contains(_))
      adj.foreach { n =>
        val newWeight = w + input.data(n.r)(n.c)
        if (newWeight < weights(n)) {
          queue.enqueue(n -> newWeight)
          weights.update(n, newWeight)
        }
      }
    }
    -1
  }

  def part1(input: RiskMap): Int = {
    dijkstra(input)
  }

  def part2(input: RiskMap): Int = {

    def extendMap(input: RiskMap): RiskMap = {
      val data =
        (0 until 5 * input.rows).map { i =>
          (0 until 5 * input.cols).map { j =>
            val value = input.data(i % input.rows)(
              j % input.cols
            ) + i / input.rows + j / input.cols
            if (value > 9) {
              if (value % 9 == 0) 9 else value % 9
            } else {
              value
            }
          }
        }
      RiskMap(data)
    }

    dijkstra(extendMap(input))
  }

  val sample = readFile("sample_15")
  assert(part1(sample) == 40)

  val items = readFile("input_15")
  println(s"Part 1: ${part1(items)}")

  assert(part2(sample) == 315)
  println(s"Part 2: ${part2(items)}")

}
