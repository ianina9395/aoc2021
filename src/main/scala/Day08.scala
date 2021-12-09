import scala.collection.mutable
import scala.io.Source

object Day08 extends App {

  def readFile(filename: String): Seq[(Seq[String], Seq[String])] =
    Source
      .fromResource(filename)
      .getLines
      .map { l =>
        l.split("\\|").toList match {
          case n1 :: n2 :: Nil =>
            n1.trim.split(" ").toSeq -> n2.trim.split(" ").toSeq
        }
      }
      .toSeq

  def part1(input: Seq[(Seq[String], Seq[String])]): Int = {
    val lengths = Seq(2, 3, 4, 7)
    input.map { case (_, n2) =>
      n2.count(n => lengths.contains(n.length))
    }.sum
  }

  def part2(input: Seq[(Seq[String], Seq[String])]): Int = {

    def decode(patterns: Seq[String], digits: Seq[String]): Int = {

      def contains(s1: String, s2: String): Boolean = {
        s2.toCharArray.toSet.subsetOf(s1.toCharArray.toSet)
      }

      val pOfLen = patterns.groupBy(_.length)
      val codes = mutable.Map[Int, String]()
      codes.put(1, pOfLen(2).head)
      codes.put(4, pOfLen(4).head)
      codes.put(7, pOfLen(3).head)
      codes.put(8, pOfLen(7).head)

      codes.put(3, pOfLen(5).find(s => contains(s, codes(7))).get)
      codes.put(9, pOfLen(6).find(s => contains(s, codes(3))).get)
      codes.put(
        0,
        pOfLen(6).find(s => contains(s, codes(7)) && !contains(s, codes(3))).get
      )
      codes.put(6, pOfLen(6).find(s => s != codes(0) && s != codes(9)).get)
      codes.put(5, pOfLen(5).find(s => contains(codes(6), s)).get)
      codes.put(2, pOfLen(5).find(s => s != codes(5) && s != codes(3)).get)

      digits
        .flatMap(d =>
          codes.filter { case (_, v) =>
            v.toCharArray.sorted sameElements d.toCharArray.sorted
          }.keys
        )
        .mkString
        .toInt
    }

    input.map { case (patterns, digits) =>
      decode(patterns, digits)
    }.sum
  }

  val sample = readFile("sample_08")
  assert(part1(sample) == 26)
  assert(part2(sample) == 61229)

  val items = readFile("input_08")
  println(s"Part 1: ${part1(items)}")
  println(s"Part 2: ${part2(items)}")

}
