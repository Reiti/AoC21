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

    val t = polymer.groupBy(identity).view.mapValues(l => l.length.toLong).toMap :: polymer.sliding(2).map(w => count(w, productions, 40)).toList
    val finalCount = t.reduce((m1, m2) => (m1.toSeq ++ m2).groupMap(_._1)(_._2).view.mapValues(_.sum).toMap)

    //Part 2
    println(finalCount.maxBy(_._2)._2 - finalCount.minBy(_._2)._2)
  }

  @tailrec
  def step(polymer: String, productions: Map[String, String], res: String): String = polymer match {
    case p if p.length < 1 => res
    case p =>
      val c = polymer.take(2)
      if(productions.contains(c)) {
        step(polymer.drop(1), productions, res + c(0) + productions(c))
      } else {
        step(polymer.drop(1), productions, res + c(0))
      }
  }

  lazy val count: ((String, Map[String, String], Int)) => Map[Char, Long] = Util.memoize {
    case (poly, prod, steps) if steps == 0 || !prod.contains(poly) => Map()
    case (poly, prod, steps) =>
      val c = prod(poly)
      val p1 = poly(0) + c
      val p2 = c + poly(1)

      val m1 = count(p1, prod, steps - 1)
      val m2 = count(p2, prod, steps - 1)
      val sub = (m1.toSeq ++ m2).groupMap(_._1)(_._2).view.mapValues(_.sum).toMap

      sub.updated(c(0), sub.getOrElse(c(0), 0L) + 1L)
  }
}
