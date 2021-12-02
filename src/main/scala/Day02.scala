import scala.annotation.tailrec
import scala.io.Source

trait Position {
  def up(v: Int): Position
  def down(v: Int): Position
  def forward(v: Int): Position
  def result: Int
}

case class Pos(x: Int, d: Int) extends Position {
  def up(v: Int): Pos = Pos(x, d - v)
  def down(v: Int): Pos = Pos(x, d + v)
  def forward(v: Int): Pos = Pos(x + v, d)
  def result: Int = x * d
}

case class Pos2(x: Int, d: Int, aim: Int) extends Position {
  def up(v: Int): Pos2 = Pos2(x, d, aim - v)
  def down(v: Int): Pos2 = Pos2(x, d, aim + v)
  def forward(v: Int): Pos2 = Pos2(x + v, d + aim * v, aim)
  def result: Int = x * d
}

object Day02 extends App {

  def readFile(filename: String): Seq[(String, Int)] =
    Source
      .fromResource(filename)
      .getLines
      .map { l =>
        val step = l.split(" ")
        (step(0), step(1).toInt)
      }
      .toSeq

  def run(start: Position, steps: Seq[(String, Int)]): Position = {

    def step(pos: Position, dir: (String, Int)): Position = {
      dir match {
        case ("up", st) =>
          pos.up(st)
        case ("down", st) =>
          pos.down(st)
        case ("forward", st) =>
          pos.forward(st)
      }
    }

    steps.foldLeft(start)((pos, dir) => step(pos, dir))
  }

  val items: Seq[(String, Int)] = readFile("input_02")
  val res1 = run(Pos(0, 0), items)
  println(s"Part 1: ${res1.result}")

  val res2 = run(Pos2(0, 0, 0), items)
  println(s"Part 2: ${res2.result}")

}
