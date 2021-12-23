import util.Util

import scala.annotation.tailrec

object Day23:
  val room = Map(3 -> 'A', 5 -> 'B', 7 -> 'C', 9 -> 'D')
  val roomC = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
  val costs = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)

  case class State(a: Set[(Int, Int)], b: Set[(Int, Int)], c: Set[(Int, Int)], d: Set[(Int, Int)])

  def main(args: Array[String]): Unit =
    //part1()
    part2()


    /*
    val n1 = neighbors(blankMap)(g, init)

    //printMap(blankMap, n1(2)._1)

    val n2 = neighbors(blankMap)(g, n1(2)._1)

    //printMap(blankMap, n2(3)._1)

    val n3 = neighbors(blankMap)(g, n2(3)._1)

    val n4 = neighbors(blankMap)(g, n3(6)._1)

    val n5 = neighbors(blankMap)(g, n4.head._1)

    val n6 = neighbors(blankMap)(g, n5(3)._1)

    val n7 = neighbors(blankMap)(g, n6.head._1)

    val n8 = neighbors(blankMap)(g, n7.head._1)

    val n9 = neighbors(blankMap)(g, n8.head._1)

    val n10 = neighbors(blankMap)(g, n9.head._1)

    val t = State(Set((2, 3), (3, 3)), Set((2, 5), (3, 5)), Set((2, 7), (3, 7)), Set((2, 9), (3, 9)))
    println(n10.head._1)
    println(t)
    println(t == n10.head._1)
    */


/*
    val n = neighbors(blankMap)(g, init)
    printMap(blankMap, init)
    println("-------------------------")
    n.foreach(nb =>
      printMap(blankMap, nb._1)
      println("------------")
    )
*/


  def part2(): Unit =
    val lines = Util.loadDayLines(23)
    val full = parseInput(lines.take(3) ++ List("  #D#C#B#A#", "  #D#B#A#C#") ++ lines.drop(3))
    val A = full.filter(_._2 == 'A').keys.toSet
    val B = full.filter(_._2 == 'B').keys.toSet
    val C = full.filter(_._2 == 'C').keys.toSet
    val D = full.filter(_._2 == 'D').keys.toSet

    val init = State(A, B, C, D)
    val blankMap = full.map(e => if e._2 == '#' || e._2 == '.' || e._2 == ' ' then e else (e._1, '.')).withDefaultValue(' ')

    printMap(blankMap, init)


  def part1(): Unit =
    val map = parseInput(Util.loadDayLines(23))
    val A = map.filter(_._2 == 'A').keys.toSet
    val B = map.filter(_._2 == 'B').keys.toSet
    val C = map.filter(_._2 == 'C').keys.toSet
    val D = map.filter(_._2 == 'D').keys.toSet

    val init = State(A, B, C, D)
    val blankMap = map.map(e => if e._2 == '#' || e._2 == '.' || e._2 == ' ' then e else (e._1, '.')).withDefaultValue(' ')


    val g: Util.WeightedGraph[State] = Map()

    val r = Util.dijkstra(g, init, State(Set((2, 3), (3, 3)), Set((2, 5), (3, 5)), Set((2, 7), (3, 7)), Set((2, 9), (3, 9))), neighbors(blankMap))

    println(r.distance)

  def parseInput(lines: List[String]): Map[(Int, Int), Char] =
    (
      for
        i <- lines.indices
        j <- lines(i).indices
      yield
        (i, j) -> lines(i)(j)
    ).toMap.withDefaultValue(' ')

  def printMap(map: Map[(Int, Int), Char], s: State): Unit =
    (0 to map.keys.maxBy(_._1)._1) foreach { j =>
      (0 to map.keys.maxBy(_._2)._2) foreach { i=>
        val pos = (j, i)
        if s.a contains pos then
          print('A')
        else if s.b contains pos then
          print('B')
        else if s.c contains pos then
          print('C')
        else if s.d contains pos then
          print('D')
        else
          print(map((j, i)))
      }
      println()
    }

  def neighbors(map: Map[(Int, Int), Char])(wg: Util.WeightedGraph[State], state: State): List[(State, Int)] =
    val a = state.a.map(s => possibleMoves(s, 'A', state.copy(a = state.a.filter(_ != s)), map)).toList
    val b = state.b.map(s => possibleMoves(s, 'B', state.copy(b = state.b.filter(_ != s)), map)).toList
    val c = state.c.map(s => possibleMoves(s, 'C', state.copy(c = state.c.filter(_ != s)), map)).toList
    val d = state.d.map(s => possibleMoves(s, 'D', state.copy(d = state.d.filter(_ != s)), map)).toList
    (a ++ b ++ c ++d).flatten

  def possibleMoves(p: (Int, Int), t: Char, state: State, map: Map[(Int, Int), Char]): List[(State, Int)] =
    val f = flatten(map, state)
    val un = unobstructed(p, f, Set())
    val legal = un.filter(u => {
      if p._2 == roomC(t) then
        if p._1 == 3 then
          false
        else if p._1 == 2 && f(3, roomC(t)) == t then
          false
        else if u._1 == 1  && !roomC.values.toList.contains(u._2) then
          true
        else if roomC(t) != u._2 then
          false
        else
          false
      else if u._2 == roomC(t) then
        if u._1 == 3 then
          true
        else if u._1 == 2 && f(3, roomC(t)) == t then
          true
        else
          false
      else
        if p._1 == 1 then
          false
        else if u._1 != 1 then
          false
        else
          !roomC.values.toList.contains(u._2)
    })
    legal.map(r =>
      if t == 'A' then
        (state.copy(a = Set((r._1, r._2)) union state.a), r._3 * 1)
      else if t == 'B' then
        (state.copy(b = Set((r._1, r._2)) union state.b), r._3 * 10)
      else if t == 'C' then
        (state.copy(c = Set((r._1, r._2)) union state.c), r._3 * 100)
      else
        (state.copy(d = Set((r._1, r._2)) union state.d), r._3 * 1000)
    )

  @tailrec
  def belowFilled(targetPos: (Int, Int), t: Char, f: Map[(Int, Int), Char]): Boolean = {
    val below = f((targetPos._1 + 1, targetPos._2))
    if below == '#' then
      true
    else if below != t then
      false
    else
      belowFilled((targetPos._1 + 1, targetPos._2), t, f)
  }

  def flatten(map: Map[(Int, Int), Char], s: State): Map[(Int, Int), Char] =
    val keys = s.a.map(_ -> 'A') ++ s.b.map(_ -> 'B') ++ s.c.map(_ -> 'C') ++ s.d.map(_ -> 'D')
    map ++ keys

  def unobstructed(p: (Int, Int), map: Map[(Int, Int), Char], visited: Set[(Int, Int)]): List[(Int, Int, Int)] =
    val nb = Util.vonNeumannNeighborhood.map(n =>
      (p._1 + n._1, p._2 + n._2)
    ).filter(p => map(p) == '.').filter(p => !visited.contains(p)).toList.map(p => (p._1, p._2, 1))

    nb ++ nb.flatMap(n => unobstructed((n._1, n._2), map, visited ++ nb.map(n => (n._1, n._2)))).map(p => (p._1, p._2, p._3 + 1))

  /*
  def heuristic(s: State) = {
    s.a.count(p => !roomC.contains(p._2))
  }
  */
