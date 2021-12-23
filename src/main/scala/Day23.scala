import util.Util

import scala.annotation.tailrec

object Day23:
  val room = Map(3 -> 'A', 5 -> 'B', 7 -> 'C', 9 -> 'D')
  val roomC = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
  val costs = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)

  case class State(a: Set[(Int, Int)], b: Set[(Int, Int)], c: Set[(Int, Int)], d: Set[(Int, Int)])

  def main(args: Array[String]): Unit =
    val lines = Util.loadDayLines(23)
    val g: Util.WeightedGraph[State] = Map()

    val (init, blankMap) = initialize(lines)
    val target = State(
      Set((2, 3), (3, 3)),
      Set((2, 5), (3, 5)),
      Set((2, 7), (3, 7)),
      Set((2, 9), (3, 9))
    )

    val r = Util.dijkstra(g, init, target, neighbors(blankMap))

    //Part 1
    println(r.distance)

    val (init2, blankMap2) = initialize(lines.take(3) ++ List("  #D#C#B#A#", "  #D#B#A#C#") ++ lines.drop(3))
    val target2 = State(
      Set((2, 3), (3, 3), (4, 3), (5, 3)),
      Set((2, 5), (3, 5), (4, 5), (5, 5)),
      Set((2, 7), (3, 7), (4, 7), (5, 7)),
      Set((2, 9), (3, 9), (4, 9), (5, 9))
    )

    val r2 = Util.dijkstra(g, init2, target2, neighbors(blankMap2))

    //Part 2
    println(r2.distance)

  def initialize(lines: List[String]): (State, Map[(Int, Int), Char]) =
    val m = parseInput(lines)
    val A = m.filter(_._2 == 'A').keys.toSet
    val B = m.filter(_._2 == 'B').keys.toSet
    val C = m.filter(_._2 == 'C').keys.toSet
    val D = m.filter(_._2 == 'D').keys.toSet
    val init = State(A, B, C, D)
    val blankMap = m.map(e => if e._2 == '#' || e._2 == '.' || e._2 == ' ' then e else (e._1, '.')).withDefaultValue(' ')
    (init, blankMap)




  def parseInput(lines: List[String]): Map[(Int, Int), Char] =
    (
      for
        i <- lines.indices
        j <- lines(i).indices
      yield
        (i, j) -> lines(i)(j)
    ).toMap.withDefaultValue(' ')

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
        if belowFilled(p, t, f) then
          false
        else if u._1 == 1  && !roomC.values.toList.contains(u._2) then
          true
        else if roomC(t) != u._2 then
          false
        else
          false
      else if u._2 == roomC(t) then
        if belowFilled((u._1, u._2), t, f) then
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
