import util.Util

import scala.annotation.tailrec

object Day10 {
  val scoring1 = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val scoring2 = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  val inv = Map('(' -> ')', '<' -> '>', '[' -> ']', '{' -> '}')

  def main(args: Array[String]): Unit = {
    val lines = Util.loadDayLines(10)

    val solved = lines.map(line => solve(line.toList, List()))

    //Part 1
    println(solved.collect({case Right(v) => v}).sum)

    val scores2 = solved.collect({case Left(v) => v}).map(_.foldLeft(0L)((acc, curr) => acc*5L + curr)).sorted

    //Part 2
    println(scores2(scores2.length/2))
  }

  @tailrec
  def solve(line: List[Char], parens: List[Char]): Either[List[Int], Int] = line match {
    case x :: xs => x match {
      case '<' | '[' | '{' | '(' => solve(xs, inv(x) :: parens)
      case c => if c != parens.head then Right(scoring1(c)) else solve(xs, parens.tail)
    }
    case Nil => if parens.isEmpty then Right(0) else Left(parens map scoring2)
  }
}
