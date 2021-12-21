import util.Util

import scala.annotation.tailrec

object Day21:
  def main(args: Array[String]): Unit =
    val (p1, p2) = (1, 2)

    //Part 1
    println(playDeterministic(p1, p2))

    //Part 2
    println(max(playQuantum(p1, p2)))

  def playDeterministic(p1: Int, p2: Int): Int =
    @tailrec
    def play(p1: Int, p2: Int, s1: Int, s2: Int, die: Int, rolled: Int, t1: Boolean): Int =
      if s1>= 1000 then
        s2 * rolled
      else if s2 >= 1000 then
        s1 * rolled
      else
        val (steps, newD) = roll(die, 3)
        if t1 then
          val pos = (p1 + steps) % 10
          play(pos, p2, s1 + pos + 1, s2, newD, rolled + 3, false)
        else
          val pos = (p2 + steps) % 10
          play(p1, pos, s1, s2 + pos + 1, newD, rolled + 3, true)
    play(p1 - 1, p2 - 1, 0, 0, 0, 0, true)

  def roll(die: Int, times: Int): (Int, Int) =
    @tailrec
    def roll(die: Int, times: Int, sum: Int): (Int, Int) =
      if times == 0 then
        (sum, die)
      else
        roll((die + 1) % 100, times - 1, sum + die + 1)
    roll(die, times, 0)

  def playQuantum(p1: Int, p2: Int): (BigInt, BigInt) =
    lazy val playQuantum: ((Int, Int, Int, Int, Int, Boolean, Int)) => (BigInt, BigInt) = Util.memoize {
      case (p1, p2, s1, s2, rollsLeft, t1, rolled) =>
        if rollsLeft == 0 then
          if t1 && s1 + ((p1 + rolled) % 10) + 1 >= 21 then
            (BigInt(1), BigInt(0))
          else if !t1 && s2 + ((p2 + rolled) % 10) + 1 >= 21 then
            (BigInt(0), BigInt(1))
          else if t1 then
            val pos = (p1 + rolled) % 10
            playQuantum(pos, p2, s1 + pos + 1, s2, 3, false, 0)
          else
            val pos = (p2 + rolled) % 10
            playQuantum(p1, pos, s1, s2 + pos + 1, 3, true, 0)
        else if t1 then
          (1 to 3)
            .map(r => playQuantum(p1, p2, s1, s2, rollsLeft - 1, true, rolled + r))
            .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
        else
          (1 to 3)
            .map(r => playQuantum(p1, p2, s1, s2, rollsLeft - 1, false, rolled + r))
            .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    }
    playQuantum(p1 - 1, p2 - 1, 0, 0, 3, true, 0)

  def max(t: (BigInt, BigInt)): BigInt = if t._1 >= t._2 then t._1 else t._2