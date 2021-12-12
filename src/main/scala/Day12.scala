import util.Util

import scala.collection.mutable

object Day12 {
  def main(args: Array[String]): Unit = {
    val input = Util.loadDayLines(12)

    val graph =  buildGraph(input.map(e => {
      val a = e.split("-")
      (a(0), a(1))
    }))

    //Part 1
    println(uniquePaths(graph, Set(), "start").count(_.contains("end")))

    //Part 2
    println(uniquePaths2(graph, List(), "start").count(_.contains("end")))
  }

  def uniquePaths(graph: Map[String, List[String]], visited: Set[String], curr: String): List[List[String]]= {
    val next = graph.filter(e => e._1 == curr).values.toList.flatten.distinct.filter(v => !visited.contains(v))
    val nextVisited = if curr(0).isLower then visited + curr else visited

    if(next.isEmpty || curr == "end") {
      List(List(curr))
    } else {
      val pathsFromHere = next.flatMap(node => uniquePaths(graph, nextVisited, node))
      pathsFromHere.map(path => curr :: path)
    }
  }

  def uniquePaths2(graph: Map[String, List[String]], visited: List[String], curr: String): List[List[String]]= {
    if(visited.distinct.size < visited.size - 1) {
      List(List())
    } else {
      val next = graph.filter(e => e._1 == curr).values.toList.flatten.distinct.filter(v => {
        if(v == "end" || v == "start") {
          visited.count(_ == v) < 1
        } else {
          visited.count(_ == v) < 2
        }
      })

      val nextVisited = if (curr(0).isLower) {
        curr :: visited
      } else {
        visited
      }

      if (next.isEmpty || curr == "end") {
        List(List(curr))
      } else {
        val pathsFromHere = next.flatMap(node => uniquePaths2(graph, nextVisited, node))
        pathsFromHere.map(path => curr :: path)
      }
    }
  }

  def buildGraph(input: List[(String, String)]): Map[String, List[String]] = {
    val uniques = input.flatMap(e => List(e._1, e._2)).distinct

    uniques.map(n => n -> input.filter(p => p._1 == n || p._2 == n).map(e => {
      if(e._1 == n) {
        e._2
      } else {
        e._1
      }
    })).toMap
  }
}
