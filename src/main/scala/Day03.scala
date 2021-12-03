import scala.annotation.tailrec
import scala.io.Source

object Day03 extends App {

  def readFile(filename: String): Seq[Array[Char]] =
    Source.fromResource(filename).getLines.map(_.toCharArray).toSeq

  def part1(numbers: Seq[Array[Char]]): Int = {
    val gamma: Array[Char] = new Array[Char](numbers.head.length)
    val eps: Array[Char] = new Array[Char](numbers.head.length)
    for (i <- numbers.head.indices) {
      val zeros = numbers.map(_(i)).count(_ == '0')
      val ones = numbers.length - zeros
      if (zeros > ones) {
        gamma(i) = '0'
        eps(i) = '1'
      } else {
        gamma(i) = '1'
        eps(i) = '0'
      }
    }
    Integer.parseInt(gamma.mkString, 2) * Integer.parseInt(eps.mkString, 2)
  }

  def part2(numbers: Seq[Array[Char]]): Int = {

    def oxygenRule(zeros: Int, ones: Int): Boolean = zeros > ones

    def co2Rule(zeros: Int, ones: Int): Boolean = zeros <= ones

    @tailrec
    def calc(
        numbers: Seq[Array[Char]],
        i: Int,
        rule: (Int, Int) => Boolean
    ): Int = {
      if (numbers.length == 1)
        Integer.parseInt(numbers.head.mkString, 2)
      else {
        val zeros = numbers.map(_(i)).count(_ == '0')
        val ones = numbers.length - zeros
        if (rule(zeros, ones)) {
          calc(numbers.filter(_(i) == '0'), i + 1, rule)
        } else {
          calc(numbers.filter(_(i) == '1'), i + 1, rule)
        }
      }
    }

    calc(numbers, 0, oxygenRule) * calc(numbers, 0, co2Rule)
  }

  val sample = readFile("sample_03")
  assert(part1(sample) == 198)

  val items = readFile("input_03")
  println(s"Part 1: ${part1(items)}")

  assert(part2(sample) == 230)
  println(s"Part 2: ${part2(items)}")

}
