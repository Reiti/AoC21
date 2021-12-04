import util.Util

import scala.annotation.tailrec

object Day4 {
  def main(args: Array[String]): Unit = {
    val input = Util.loadDayLines(4).toArray
    val numbers = input.head.split(",").map(_.toInt)

    val bingos = input.tail.filter(_.length > 1).map(_.trim.split(" +").map(_.toInt)).grouped(5).toList

    //Part 1
    println(firstWinner(bingos, numbers))

    //Part 2
    println(lastWinner(bingos, numbers))
  }

  @tailrec
  def firstWinner(current: List[Array[Array[Int]]], numbers: Array[Int]): Int = {
    val next = current.map(bingo => play(bingo, numbers.head))

    next.find(bingo => won(bingo)) match {
      case Some(bingo) => numbers.head * bingo.map(_.filter(_ != -1).sum).sum
      case None => firstWinner(next, numbers.tail)
    }
  }

  @tailrec
  def lastWinner(current: List[Array[Array[Int]]], numbers: Array[Int]): Int = {
    val remaining = current.map(bingo => play(bingo, numbers.head)).filterNot(won)

    if(remaining.isEmpty) {
      numbers.head * play(current.head, numbers.head).map(_.filter(_ != -1).sum).sum
    } else {
      lastWinner(remaining, numbers.tail)
    }
  }

  def play(bingo: Array[Array[Int]], number: Int): Array[Array[Int]] = {
    bingo.map(_.map(v => if v == number then -1 else v))
  }

  def won(bingo: Array[Array[Int]]): Boolean = {
    bingo.count(_.count(_ == -1) == 5) > 0 || bingo.transpose.count(_.count(_ == -1) == 5) > 0
  }
}
