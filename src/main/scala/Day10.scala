import scala.collection.mutable
import scala.io.Source

object Day10 extends App {

  def readFile(filename: String): Seq[String] =
    Source.fromResource(filename).getLines.filterNot(_.trim.isEmpty).toSeq

  def invalidScore(s: String): Int = {

    val scores = Map(
      ")" -> 3,
      "]" -> 57,
      "}" -> 1197,
      ">" -> 25137
    )

    def isSameType(b1: String, b2: String): Boolean = {
      b1 == "(" && b2 == ")" ||
      b1 == "[" && b2 == "]" ||
      b1 == "{" && b2 == "}" ||
      b1 == "<" && b2 == ">"
    }

    val braces = s.split("").toSeq
    val stack = mutable.Stack[String]()

    val openBraces = Set("(", "[", "{", "<")
    for (b <- braces) {
      if (openBraces.contains(b)) {
        stack.push(b)
      } else {
        val lastOpen = stack.pop()
        if (!isSameType(lastOpen, b))
          return scores(b)
      }
    }
    0
  }

  def part1(input: Seq[String]): Int = {
    input.map(invalidScore).sum
  }

  def part2(input: Seq[String]): Long = {

    def incompleteScore(s: String): Long = {

      val scores = Map(
        ")" -> 1,
        "]" -> 2,
        "}" -> 3,
        ">" -> 4
      )

      val braces = s.split("").toSeq

      val stack = mutable.Stack[String]()
      val openBraces = Set("(", "[", "{", "<")
      for (b <- braces) {
        if (openBraces.contains(b)) {
          stack.push(b)
        } else {
          stack.pop()
        }
      }

      var score = 0L
      if (stack.nonEmpty) {
        val missingClosed = stack.popAll().reverse

        for (mc <- missingClosed) {
          score = score * 5
          score += (mc match {
            case "(" => scores(")")
            case "[" => scores("]")
            case "{" => scores("}")
            case "<" => scores(">")
          })
        }
      }
      score
    }

    val scores = input.filter(invalidScore(_) == 0).map(incompleteScore).sorted
    scores(scores.length / 2)
  }

  val sample = readFile("sample_10")
  assert(part1(sample) == 26397)

  val items = readFile("input_10")
  println(s"Part 1: ${part1(items)}")

  assert(part2(sample) == 288957)
  println(s"Part 2: ${part2(items)}")

}
