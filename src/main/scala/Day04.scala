import scala.collection.mutable

case class Num(n: Int, marked: Boolean = false)

class Board(numbers: Array[Array[Num]]) {
  var last = 0
  var isWinning = false

  private def checkWinning: Boolean = {
    numbers.exists(r => r.count(_.marked) == r.length) ||
    numbers.transpose.exists(r => r.count(_.marked) == r.length)
  }

  def markIfFound(num: Int): Unit = {
    for (i <- numbers.indices; j <- numbers.head.indices) {
      if (numbers(i)(j).n == num) {
        numbers(i)(j) = numbers(i)(j).copy(marked = true)
        last = num
      }
    }
    isWinning = checkWinning
  }

  def score: Int = {
    numbers.map(_.filterNot(_.marked).map(_.n).sum).sum * last
  }
}

object Day04 extends App {

  def readFile(filename: String): (Seq[Int], Seq[Board]) = {
    val f = FileReader.readBlocks(filename)
    val numbers = f.head.split(",").map(_.toInt).toSeq
    val boards = f.tail.map { b =>
      val rows = b.split("\n")
      new Board(
        rows.map(_.split(" ").filterNot(_.isEmpty).map(s => Num(s.toInt)))
      )
    }
    numbers -> boards
  }

  def part1(numbers: Seq[Int], boards: Seq[Board]): Int = {
    val it = numbers.iterator

    while (!boards.exists(_.isWinning) && it.hasNext) {
      val n = it.next()
      boards.foreach(_.markIfFound(n))
    }

    boards.find(_.isWinning).get.score
  }

  def part2(numbers: Seq[Int], boards: Seq[Board]): Int = {
    val winners = new mutable.ArrayBuffer[Board]()
    for (n <- numbers) {
      boards.filterNot(_.isWinning).foreach(_.markIfFound(n))

      boards.filter(_.isWinning).foreach { w =>
        if (!winners.contains(w)) {
          winners.append(w)
        }
      }
    }
    winners.last.score
  }

  val (ns, sample) = readFile("sample_04")
  assert(part1(ns, sample) == 4512)

  val (n, input) = readFile("input_04")
  println(s"Part 1: ${part1(n, input)}")

  assert(part2(ns, sample) == 1924)
  println(s"Part 2: ${part2(n, input)}")
}
