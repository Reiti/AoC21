import util.Util

import scala.annotation.tailrec

object Day10 {
  val scoring1 = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val scoring2 = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  val inv = Map('(' -> ')', '<' -> '>', '[' -> ']', '{' -> '}')

  def main(args: Array[String]): Unit = {
    val lines = Util.loadDayLines(10)

    println(lines.map(line => findCorrupted(line, List()) match {
      case Some(c) => scoring1(c)
      case None => 0
    }).sum)

    val scores = lines.map(line => completeIncomplete(line, List()).foldLeft(0L)({ case (score, next) => score * 5L + scoring2(next) })).filter(_ != 0).sorted

    println(scores(scores.length/2))
  }

  @tailrec
  def findCorrupted(line: String, parens: List[Char]): Option[Char] = {
    if(line.isEmpty) {
      None
    }
    else {
      line.head match {
        case '<' | '[' | '{' | '(' => findCorrupted(line.tail, inv(line.head) :: parens );
        case p =>
          if (parens.head != line.head) {
            Some(line.head)
          } else {
            findCorrupted(line.tail, parens.tail)
          }
      }
    }
  }

  @tailrec
  def completeIncomplete(line: String, parens: List[Char]): String = {
    if(line.isEmpty) {
      parens.mkString
    } else {
      line.head match {
        case '<' | '[' | '{' | '(' => completeIncomplete(line.tail, inv(line.head) :: parens);
        case p =>
          if (parens.head != line.head) {
            ""
          } else {
            completeIncomplete(line.tail, parens.tail)
          }
      }
    }
  }
}
