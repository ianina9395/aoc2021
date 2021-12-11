import scala.io.Source

case class MapPoint(r: Int, c: Int)

class OctopusMap(var data: Array[Array[Int]]) {
  val rows = data.length
  val cols = data.head.length

  def allPoints: Seq[MapPoint] = {
    for (i <- 0 until rows; j <- 0 until cols) yield MapPoint(i, j)
  }

  def adj(p: MapPoint): Seq[MapPoint] = {

    def checkValid(p: MapPoint): Boolean = {
      p.r <= (rows - 1) && p.r >= 0 && p.c <= (cols - 1) && p.c >= 0
    }

    (for (i <- p.r - 1 to p.r + 1; j <- p.c - 1 to p.c + 1)
      yield MapPoint(i, j)).filter(checkValid).filterNot(_ == p)
  }

  def isFlashing(p: MapPoint): Boolean = data(p.r)(p.c) > 9

  def runStep(): Int = {

    def runFlashes(): Int = {
      val flushing = allPoints.filter(isFlashing)
      flushing.foreach(p => data(p.r)(p.c) = 0)
      flushing.size
    }

    def incEnergy(flushing: Seq[MapPoint]): Boolean = {
      val toBeInc = flushing
        .flatMap(adj)
        .filterNot(flushing.contains)

      toBeInc.foreach(p => data(p.r)(p.c) += 1)
      toBeInc.exists(isFlashing)
    }

    data = data.map(_.map { p => p + 1 })

    var flushed = Set[MapPoint]()
    var toBeFlushed = false
    do {
      val flushing = allPoints
        .filter(isFlashing)
        .filterNot(flushed.contains)

      flushed ++= flushing
      toBeFlushed = incEnergy(flushing)
    } while (toBeFlushed)

    runFlashes()
  }
}

object Day11 extends App {

  def readFile(filename: String): OctopusMap = {
    val data = Source
      .fromResource(filename)
      .getLines
      .filterNot(_.trim.isEmpty)
      .map(_.split("").map(_.toInt))
      .toArray
    new OctopusMap(data)
  }

  def part1(input: OctopusMap, steps: Int): Int = {
    val map = new OctopusMap(input.data.map(_.clone()))

    (0 until steps).map { _ =>
      map.runStep()
    }.sum
  }

  def part2(input: OctopusMap): Int = {
    val map = new OctopusMap(input.data.map(_.clone()))
    var step = 1
    var flashed = 0
    do {
      flashed = map.runStep()
      step += 1
    } while (flashed < map.cols * map.rows)
    step - 1
  }

  val sample = readFile("sample_11")
  assert(part1(sample, 10) == 204)
  assert(part1(sample, 100) == 1656)

  val items = readFile("input_11")
  println(s"Part 1: ${part1(items, 100)}")

  assert(part2(sample) == 195)
  println(s"Part 2: ${part2(items)}")

}
