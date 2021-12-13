case class Coord(x: Int, y: Int)
case class Fold(coord: String, num: Int)

object Day13 extends App {

  def readFile(filename: String): (Seq[Coord], Seq[Fold]) = {
    val b = FileReader.readBlocks(filename)
    (
      b.head
        .split("\n")
        .map { p =>
          val xy = p.split(",", 2)
          Coord(xy(0).toInt, xy(1).toInt)
        }
        .toSeq,
      b(1)
        .split("\n")
        .map { f =>
          val cn = f.replace("fold along ", "").split("=", 2)
          Fold(cn(0), cn(1).toInt)
        }
        .toSeq
    )
  }

  private def doFold(p: Coord, fold: Fold): Coord = {
    def foldByY(p: Coord, y: Int): Coord = {
      if (p.y < y) p
      else Coord(p.x, 2 * y - p.y)
    }

    def foldByX(p: Coord, x: Int): Coord = {
      if (p.x < x) p
      else Coord(2 * x - p.x, p.y)
    }

    if (fold.coord == "x")
      foldByX(p, fold.num)
    else
      foldByY(p, fold.num)
  }

  def part1(input: Seq[Coord], folds: Seq[Fold]): Int = {
    val firstFold = folds.head
    input.map(p => doFold(p, firstFold)).distinctBy(identity).size
  }

  def part2(input: Seq[Coord], folds: Seq[Fold]): Unit = {

    def range(coords: Iterable[Int]): Range = Range(coords.min, coords.max)

    val points = folds
      .foldLeft(input)((points, f) => points.map(p => doFold(p, f)))
      .distinctBy(identity)

    val rows = points.groupBy(_.y)
    val yRange = range(rows.keys)
    val xRange = range(rows.flatMap { case (_, points) => points.map(_.x) })

    for (y <- yRange) {
      val row = xRange.map { x =>
        if (points.contains(Coord(x, y)))
          "#"
        else
          "."
      }.mkString
      println(row)
    }
  }

  val (sample, sampleFolds) = readFile("sample_13")
  assert(part1(sample, sampleFolds) == 17)

  val (items, folds) = readFile("input_13")

  println(s"Part 1: ${part1(items, folds)}")

  part2(sample, sampleFolds)
  part2(items, folds)

}
