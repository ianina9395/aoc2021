import scala.io.Source

object Day01 extends App {

  def readFile(filename: String): Seq[Int] =
    Source.fromResource(filename).getLines.map(_.toInt).toSeq

  def countIncrements(measurements: Seq[Int]): Int = {
    measurements.zipWithIndex.tail.map { case (m, i) =>
      if (m > measurements(i - 1))
        1
      else
        0
    }.sum
  }

  def countIncrementsWindows(measurements: Seq[Int]): Int = {
    countIncrements(
      measurements.sliding(3).map(_.sum).toSeq
    )
  }

  val sample = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
  assert(countIncrements(sample) == 7)

  val items: Seq[Int] = readFile("input_01")
  println(s"Part 1: ${countIncrements(items)}")
  println(s"Part 2: ${countIncrementsWindows(items)}")

}
