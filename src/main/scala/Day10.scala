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
    println(solved.collect({case Right(v) => v}).map({
      case Some(c) => scoring1(c)
      case None => 0
    }).sum)

    val scores2 = solved.collect({case Left(v) => v}).filter(_.nonEmpty).map(_.foldLeft(0L)({case (score, next) => score * 5L + scoring2(next)})).sorted

    //Part 2
    println(scores2(scores2.length/2))
  }

  @tailrec
  def solve(line: List[Char], parens: List[Char]): Either[String, Option[Char]] = line match {
    case x :: xs => x match {
      case '<' | '[' | '{' | '(' => solve(xs, inv(x) :: parens)
      case c => if c != parens.head then Right(Some(c)) else solve(xs, parens.tail)
    }
    case Nil => if parens.isEmpty then Right(None) else Left(parens.mkString)
  }
}
