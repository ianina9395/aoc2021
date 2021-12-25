import scala.io.Source

object Day25 extends App {

  case class Pos(r: Int, c: Int)

  class Seafloor(var data: Array[Array[Char]]) {
    val rows = data.size
    val cols = data.head.size

    def east(p: Pos): Pos = Pos(p.r, (p.c + 1) % cols)

    def south(p: Pos): Pos = Pos((p.r + 1) % rows, p.c)

    def isFree(p: Pos): Boolean = data(p.r)(p.c) == '.'

    def move(ch: Char, p: Pos, newPos: Pos): Unit = {
        data(p.r)(p.c) = '.'
        data(newPos.r)(newPos.c) = ch
    }

    def runStep(): Boolean = {
      def moveHerd(ch: Char, dir: Pos => Pos): Boolean = {
        var moved = false
        val newState = new Seafloor(data.map(_.clone()))
        for (r <- data.indices; c <- data.head.indices) {
          if (this.data(r)(c) == ch) {
            val pos = Pos(r, c)
            val newPos = dir(pos)
            if (this.isFree(newPos)) {
              newState.move(ch, pos, newPos)
              moved = true
            }
          }
        }
        data = newState.data
        moved
      }

      val movedEast = moveHerd('>', east)
      val movedSouth = moveHerd('v', south)

      movedEast || movedSouth
    }
  }


  def readFile(filename: String): Array[Array[Char]] =
    Source.fromResource(filename).getLines.map(_.toCharArray).toArray

  def part1(input: Array[Array[Char]]): Int = {
    val seafloor = new Seafloor(input.map(_.clone()))
    var step = 0
    var moved = false
    do {
      moved = seafloor.runStep()
      step += 1
    } while (moved)
    step
  }

  val sample = readFile("sample_25")

  assert(part1(sample) == 58)

  val items = readFile("input_25")
  println(s"Part 1: ${part1(items)}")

}
