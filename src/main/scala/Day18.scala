import util.Util

import scala.annotation.tailrec

object Day18 {

  case class Node(var parent: Option[Node], var left: Option[Node], var right: Option[Node], var value: Option[Long], var isLeft: Boolean)
  def main(args: Array[String]): Unit = {
    val input = Util.loadDayLines(18)
    val parsed = input.map(l => parse(l, None))

    val finalSum = parsed.tail.foldLeft(parsed.head)((acc, next) => add(acc, next))

    //Part 1
    println(magnitude(finalSum))

    val pairs = input.flatMap(in1 => input.map(in2 => (in1, in2))).filter({case (a, b) => a != b})
    val pairParsed = pairs.map(p => (parse(p._1, None), parse(p._2, None)))

    //Part 2
    println(pairParsed.map(pair => magnitude(add(pair._1, pair._2))).max)
  }

  @tailrec
  def parse(line: String, parent: Option[Node]): Option[Node] = {
    if  (line.startsWith("[")){
      val n = Some(Node(parent, None, None, None, false))
      if (parent.nonEmpty) {
        if (parent.get.left.isEmpty) {
          n.get.isLeft = true
          parent.get.left = n
        } else
          n.get.isLeft = false
          parent.get.right = n
      }
      parse(line.tail, n)
    } else if (line.startsWith(",")) {
      parse(line.tail, parent)
    } else if (line.startsWith("]")) {
      if(line.length == 1) {
        parent
      } else {
        parse(line.tail, parent.get.parent)
      }
    } else {
      val v = line.takeWhile(c => c >= '0' && c <= '9')
      val n = Some(Node(parent, None, None, Some(v.toLong), false))

      if(parent.get.left.isEmpty) {
        n.get.isLeft = true
        parent.get.left = n
      } else {
        n.get.isLeft = false
        parent.get.right = n
      }

      parse(line.drop(v.length), parent)
    }
  }

  def printTree(curr: Node): Unit = {
    curr.value match {
      case Some(v) =>
        print(v)
      case _ =>
        print("[")
        printTree(curr.left.get)
        print(",")
        printTree(curr.right.get)
        print("]")
    }
  }

  def explode(curr: Option[Node], level: Int): Boolean = {
    if(curr.isEmpty) {
      false
    }
    else if(level == 3) {
      if(curr.get.left.isDefined && curr.get.left.get.value.isEmpty) {
        val n = curr.get.left
        val vl = n.get.left.get.value.get
        val vr = n.get.right.get.value.get
        curr.get.left = Some(Node(curr, None, None, Some(0L), true))

        explodeLeft(curr.get.left, vl)
        explodeRight(curr.get.left, vr)
        true
      } else if(curr.get.right.isDefined && curr.get.right.get.value.isEmpty) {
        val n = curr.get.right
        val vl = n.get.left.get.value.get
        val vr = n.get.right.get.value.get
        curr.get.right = Some(Node(curr, None, None, Some(0L), false))

        explodeLeft(curr.get.right, vl)
        explodeRight(curr.get.right, vr)
        true
      } else {
        false
      }
    } else {
      val e = explode(curr.get.left, level + 1)
      e || explode(curr.get.right, level +1)
    }
  }

  @tailrec
  def explodeLeft(curr: Option[Node], vl: Long): Unit = {
    val p = curr.get.parent
    if(p.isDefined) {

      if (!curr.get.isLeft) {
        explodeRightMost(p.get.left, vl)
      } else {
        explodeLeft(p, vl)
      }
    }
  }

  @tailrec
  def explodeRightMost(curr: Option[Node], vl: Long): Unit = {
    if(curr.get.right.isEmpty) {
      curr.get.value = Some(curr.get.value.get + vl)
    } else {
      explodeRightMost(curr.get.right, vl)
    }
  }

  @tailrec
  def explodeRight(curr: Option[Node], vl: Long): Unit = {
    val p = curr.get.parent
    if(p.isDefined) {
      if(curr.get.isLeft) {
        explodeLeftMost(p.get.right, vl)
      } else {
        explodeRight(p, vl)
      }
    }
  }

  @tailrec
  def explodeLeftMost(curr: Option[Node], vl: Long): Unit = {
    if(curr.get.left.isEmpty) {
      curr.get.value = Some(curr.get.value.get + vl)
    } else {
      explodeLeftMost(curr.get.left, vl)
    }
  }

  def split(cur: Option[Node]): Boolean = {
    if(cur.isEmpty) {
      false
    } else {
      cur.get.value match {
        case Some(l) if l >= 10 =>
          val left = Math.floor(l.toDouble/2)
          val right = Math.ceil(l.toDouble/2)
          cur.get.value = None
          cur.get.left = Some(Node(cur, None, None, Some(left.toLong), true))
          cur.get.right = Some(Node(cur, None, None, Some(right.toLong), false))
          true
        case _ =>
          val s = split(cur.get.left)
          s || split(cur.get.right)
      }
    }
  }

  @tailrec
  def reduce(root: Option[Node]): Option[Node] = {
    while(explode(root, 0)){}
    if(split(root)) {
      reduce(root)
    } else {
      root
    }
  }

  def add(left: Option[Node], right: Option[Node]): Option[Node] = {
    val root = Some(Node(None, left, right, None, false))

    left.get.parent = root
    left.get.isLeft = true

    right.get.parent = root
    right.get.isLeft = false

    reduce(root)
  }

  def magnitude(curr: Option[Node]): Long = curr.get.value match {
    case Some(l) => l
    case _ => 3 * magnitude(curr.get.left) + 2 * magnitude(curr.get.right)
  }
}
