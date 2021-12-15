object Day14 extends App {

  case class Rule(twoLetters: String, newLetter: Char)

  def readFile(filename: String): (String, Seq[Rule]) = {
    val b = FileReader.readBlocks(filename)
    b.head -> b(1)
      .split("\n")
      .map { l =>
        val r = l.split(" -> ", 2)
        Rule(r(0), r(1).head)
      }
      .toSeq
  }

  def runSteps(input: String, rules: Seq[Rule], steps: Int): Long = {

    def letterCounts(
        pairsCounts: Map[String, Long],
        input: String
    ): Map[Char, Long] = {
      val first = input.head
      val last = input.last

      pairsCounts.view
        .flatMap { case (pair, c) =>
          Seq(pair(0) -> c, pair(1) -> c)
        }
        .groupMapReduce(_._1)(_._2)(_ + _)
        .map {
          case (ch, c) if ch == first || ch == last =>
            ch -> (c / 2 + 1)
          case (ch, c) =>
            ch -> c / 2
        }
    }

    def runStep(pairsCounts: Map[String, Long]): Map[String, Long] = {
      pairsCounts.view
        .flatMap { case (pair, count) =>
          rules.find(_.twoLetters == pair) match {
            case Some(Rule(twoLetters, newLetter)) =>
              val newPair1 = twoLetters(0).toString + newLetter
              val newPair2 = newLetter.toString + twoLetters(1)

              Seq(newPair1 -> count, newPair2 -> count)
            case None =>
              Seq(pair -> count)
          }
        }
        .groupMapReduce(_._1)(_._2)(_ + _)
    }

    val pairs: Seq[String] =
      (0 to input.length - 2).map(i => input.substring(i, i + 2))

    val pairCounts: Map[String, Long] =
      pairs.groupMapReduce(identity)(_ => 1L)(_ + _)

    val newPairCounts = (1 to steps).foldLeft(pairCounts) { case (p, _) =>
      runStep(p)
    }

    val lc = letterCounts(newPairCounts, input)
    lc.values.max - lc.values.min
  }

  val (sample, sampleRules) = readFile("sample_14")
  assert(runSteps(sample, sampleRules, 10) == 1588)

  val (input, rules) = readFile("input_14")
  println(s"Part 1: ${runSteps(input, rules, 10)}")

  assert(runSteps(sample, sampleRules, 40) == 2188189693529L)
  println(s"Part 2: ${runSteps(input, rules, 40)}")

}
