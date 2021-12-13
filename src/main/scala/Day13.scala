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

  private def fold(p: Coord, fold: Fold): Coord = {
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
    input.map(p => fold(p, firstFold)).distinctBy(identity).size
  }

  def part2(input: Seq[Coord], folds: Seq[Fold]): Unit = {
    val points = folds
      .foldLeft(input)((points, f) => points.map(p => fold(p, f)))
      .distinctBy(identity)

    val rows = points.groupBy(_.y)
    val (yMin, yMax) = {
      val allY = rows.keys
      (allY.min, allY.max)
    }

    val (xMin, xMax) = {
      val allX = rows.flatMap(_._2.map(_.x))
      (allX.min, allX.max)
    }

    for (y <- yMin to yMax) {
      val row = (xMin to xMax).map { x =>
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
