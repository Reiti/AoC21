import util.Util

import scala.annotation.tailrec

object Day3 {
  def main(args: Array[String]): Unit = {
    val diagnostic = Util.loadDayLines(3).map(_.split("").map(_.toInt))

    val gamma = diagnostic.transpose.map(line => if line.count(_ == 1) > line.count(_ == 0) then 1 else 0)
    val epsilon = diagnostic.transpose.map(line => if line.count(_ == 1) > line.count(_ == 0) then 0 else 1)

    //Part 1
    println(Integer.parseInt(gamma.mkString, 2)*Integer.parseInt(epsilon.mkString, 2))

    //Part 2
    println(filter(diagnostic, (z, o) => if z>o then 1 else 0) * filter(diagnostic, (z, o) => if z>o then 0 else 1))
  }


  def filter(diagnostic: List[Array[Int]], keep: (Int, Int) => Int): Int = {
    @tailrec
    def filter(diagnostic: List[Array[Int]], keep: (Int, Int) => Int, pos: Int): Int = {
      if (diagnostic.size == 1) {
        Integer.parseInt(diagnostic.head.mkString, 2)
      } else {
        val zeroes = diagnostic.count(arr => arr(pos) == 0)
        val ones = diagnostic.count(arr => arr(pos) == 1)

        filter(diagnostic.filter(arr => arr(pos) == keep(zeroes, ones)), keep, pos + 1)
      }
    }
    filter(diagnostic, keep, 0)
  }
}
