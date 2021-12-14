import util.Util

import scala.annotation.tailrec

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = Util.loadDayLines(14)
    val polymer = input.head
    val productions = input.drop(2).map(line => {
      val s = line.split(" -> ")
      s(0) -> s(1)
    }).toMap


    val prod = (0 until 10).foldLeft(polymer)((acc, c) => step(acc, productions, "")).groupBy(identity).map(e => (e._1, e._2.length))

    //Part 1
    println(prod.maxBy(_._2)._2 - prod.minBy(_._2)._2)


    val t = polymer.sliding(2).map(w => count(w, productions, 40)).toList

    val finalCount = ('A' to 'Z').map(k => {
      k -> (t.map(_.getOrElse(k, BigInt(0))).sum + polymer.count(_ == k))
    }).filter(_._2 != 0).toMap

    //Part 2
    println(finalCount.maxBy(_._2)._2 - finalCount.minBy(_._2)._2)
  }

  @tailrec
  def step(polymer: String, productions: Map[String, String], res: String): String = {
    if(polymer.isEmpty) {
      res
    } else {
      val c = polymer.take(2)
      if(productions.contains(c)) {
        step(polymer.drop(1), productions, res + c(0) + productions(c))
      } else {
        step(polymer.drop(1), productions, res + c(0))
      }
    }
  }

  lazy val count: ((String, Map[String, String], Int)) => Map[Char, BigInt] = Util.memoize {
    case (poly, prod, steps) if steps == 0 =>
        Map()
    case (poly, prod, steps) =>
      if(prod.contains(poly)) {
        val c = prod(poly)

        val p1 = poly(0) + c
        val p2 = c + poly(1)

        val m1 = count(p1, prod, steps - 1)
        val m2 = count(p2, prod, steps - 1)

        val sub = ('A' to 'Z').map(k => {
          k -> (m1.getOrElse(k, BigInt(0)) + m2.getOrElse(k, BigInt(0)))
        }).filter(_._2 != 0).toMap

        sub.updated(c(0), sub.getOrElse(c(0), BigInt(0)) + BigInt(1))
      } else {
        Map()
      }
    }
}
