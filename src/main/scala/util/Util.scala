package util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Util {
  def loadDay(day: Int): String = {
    Using(Source.fromResource(s"$day.txt")) {
      source => source.mkString.strip()
    }.get
  }

  def loadDayKeepWhitespace(day: Int): String = {
    Using(Source.fromResource(s"$day.txt")) {
      source => source.mkString
    }.get
  }

  def loadFilename(name: String): String = {
    Using(Source.fromResource(name)) {
      source => source.mkString.strip()
    }.get
  }

  def loadDayLines(day: Int): List[String] = loadDay(day).split("\n").toList

  def loadDayInts(day: Int): List[Int] = loadDayLines(day).map(_.toInt)

  def loadDayMap(day: Int): Map[(Int, Int), Char] = {
    val lines = loadDayLines(day)
    lines.indices.flatMap(col => lines.head.indices.map(row => {
      (col, row) -> lines(col)(row)
    })).toMap
  }

  def loadDayProgram(day: Int): Map[Int, (String, Int)] =
    Util.loadDayLines(day)
      .map {l => 
        val split = l.split(" ")
        (split(0), split(1).toInt)
      }
      .zipWithIndex
      .map { x => (x._2, x._1) }.toMap

  @tailrec
  def gcd(a: Long, b: Long): Long = {
    if(b == 0)
      a
    else
      gcd(b, a%b)
  }

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def lcm(nums: Long*): Long = nums.reduce(lcm)

  def zip3[A](a: List[A], b: List[A], c: List[A]): List[(A, A, A)] = {
    (a zip b zip c) map {
      case ((a, b), c) => (a, b, c)
    }
  }

  def zip3[A](t: (List[A], List[A], List[A])): List[(A, A, A)] = zip3(t._1, t._2, t._3)

  def time[A](block: => A): A = {
    val t0  = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  case class Node(label: String)
  case class Graph(adj: Map[Node, Set[Node]]) {
    case class DfsState(discovered: Set[Node] = Set(), activeNodes: Set[Node] = Set(), tsOrder: List[Node] = List(),
                        isCyclic: Boolean = false)

    def dfs: (List[Node], Boolean) = {
      def dfsVisit(currState: DfsState, src: Node): DfsState = {
        val newState = currState.copy(discovered = currState.discovered + src, activeNodes = currState.activeNodes + src,
          isCyclic = currState.isCyclic || adj(src).exists(currState.activeNodes))

        val finalState = adj(src).filterNot(newState.discovered).foldLeft(newState)(dfsVisit)
        val ord = if(finalState.tsOrder.contains(src)) finalState.tsOrder else src :: finalState.tsOrder
        finalState.copy(tsOrder = ord, activeNodes = finalState.activeNodes - src)
      }

      val stateAfterSearch = adj.keys.foldLeft(DfsState()) {(state, n) => if (state.discovered(n)) state else dfsVisit(state, n)}
      (stateAfterSearch.tsOrder, stateAfterSearch.isCyclic)
    }

    def topologicalSort: Option[List[Node]] = dfs match {
      case (topologicalOrder, false) => Some(topologicalOrder)
      case _ => None
    }
  }

  type WeightedGraph[A] = Map[A, List[(A, Int)]]
  def dijkstra[A, B](graph: WeightedGraph[A], start: B, target: B, neighbors: (WeightedGraph[A], B) => List[(B, Int)]): Int = {
    val distance = scala.collection.mutable.HashMap[B, Int]().withDefault(_ => Int.MaxValue)
    val visited = scala.collection.mutable.HashSet[B]()
    val q = scala.collection.mutable.PriorityQueue[B]()(Ordering.by(c => -distance(c)))

    distance(start) = 0
    q.enqueue(start)

    while (q.nonEmpty) {
      val node = q.dequeue()
      if (node == target) {
        return distance(target)
      }
      for (neigh <- neighbors(graph, node)) {
        if (!visited.contains(neigh._1)) {
          val newDistance = distance(node) + neigh._2
          if (newDistance < distance(neigh._1)) {
            distance(neigh._1) = newDistance
            q.enqueue(neigh._1)
          }
        }
      }
      visited.add(node)
    }
    distance(target)
  }

  //https://stackoverflow.com/a/36960228
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  val vonNeumannNeighborhood: Seq[(Int, Int)] = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
  val mooreNeighborhood: Seq[(Int, Int)] = vonNeumannNeighborhood ++ Seq((-1, -1), (-1, 1), (1, -1), (1, 1))
}
