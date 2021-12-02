import util.Util

import scala.annotation.tailrec

object Day2 {
  def main(args: Array[String]): Unit = {
    val course = Util.loadDayLines(2).map(_.split(" ")).map(v => (v.head, v(1).toInt))

    val pos1 = finalPositionPart1(course)
    println(pos1._1 * pos1._2)

    val pos2 = finalPositionPart2(course)
    println(pos2._1 * pos2._2)
  }

  def finalPositionPart1(course: List[(String, Int)]): (Int, Int) = {
    @tailrec
    def step(x: Int, y: Int, course: List[(String, Int)]): (Int, Int) = course match {
      case ("forward", dx) :: xs  => step(x + dx, y, xs)
      case ("down", dy) :: xs => step(x, y + dy, xs)
      case ("up", dy) :: xs => step(x, y - dy, xs)
      case _ => (x, y)
    }
    step(0, 0, course)
  }

  def finalPositionPart2(course: List[(String, Int)]): (Int, Int) = {
    @tailrec
    def step(x: Int, y: Int, aim: Int, course: List[(String, Int)]): (Int, Int) = course match {
      case ("forward", dx) :: xs  => step(x + dx, y + aim*dx, aim, xs)
      case ("down", dy) :: xs => step(x, y, aim + dy, xs)
      case ("up", dy) :: xs => step(x, y, aim - dy, xs)
      case _ => (x, y)
    }
    step(0, 0, 0, course)
  }
}
