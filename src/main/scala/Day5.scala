import util.Util

import scala.annotation.tailrec

object Day5 {
  def main(args: Array[String]): Unit = {
    val reg = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
    val lines = Util.loadDayLines(5)map{
      case reg(x1, y1, x2, y2) => ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
      case _ => throw new RuntimeException("Malformed input!")
    }

    //Part 1
    println(mark(lines.filter(l => l._1._1 == l._2._1 || l._1._2 == l._2._2)).values.count(_ > 1))

    //Part 2
    println(mark(lines).values.count(_ > 1))
  }

  def mark(lines: List[((Int, Int), (Int, Int))]): Map[(Int, Int), Int] = {
    @tailrec
    def markH(lines: List[((Int, Int), (Int, Int))], acc: Map[(Int, Int), Int]): Map[(Int, Int), Int] = lines match {
      case x :: xs => markH(xs, markLine(x, acc))
      case Nil => acc
    }
    markH(lines, Map())
  }

  def markLine(line: ((Int, Int), (Int, Int)), map: Map[(Int, Int), Int]) : Map[(Int, Int), Int] = {
    @tailrec
    def markLineH(x: Int, y: Int, dx: Int, dy: Int, acc: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
      if(x == line._2._1 && y == line._2._2) {
        acc.updatedWith((x,y))(v => Some(v.getOrElse(0) + 1))
      } else {
        markLineH(x + dx, y + dy, dx, dy, acc.updatedWith((x,y))(v => Some(v.getOrElse(0) + 1)))
      }
    }

    val sgnX = Math.signum(line._2._1 - line._1._1)
    val sgnY = Math.signum(line._2._2 - line._1._2)
    markLineH(line._1._1, line._1._2, sgnX.toInt, sgnY.toInt, map)
  }
}
