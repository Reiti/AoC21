import util.Util

import scala.annotation.tailrec

object Day19 {
  case class Scanner(idx: Int, beacons: Set[Array[Int]], offset: Array[Int], rotation: Array[Array[Int]])

  val Identity: Array[Array[Int]] = Array(
    Array(1, 0, 0),
    Array(0, 1, 0),
    Array(0, 0, 1)
  )

  val X: Array[Array[Int]] = Array(
    Array(1, 0, 0),
    Array(0, 0, -1),
    Array(0, 1, 0)
  )

  val Y: Array[Array[Int]] = Array(
    Array(0, 0, 1),
    Array(0, 1, 0),
    Array(-1, 0, 0)
  )

  val Z: Array[Array[Int]] = Array(
    Array(0, -1, 0),
    Array(1, 0, 0),
    Array(0, 0, 1)
  )

  val allRotations: List[Array[Array[Int]]] = (for {
    i <- 0 to 3
    j <- 0 to 3
    k <- 0 to 3
  } yield times(exp(X, i), times(exp(Y, j), exp(Z, k)))).toList.distinctBy(rot => rot.map(line => line.mkString).mkString)

  def main(args: Array[String]): Unit = {
    val input = Util.loadDay(19).split("\n\n").map(parse).toList

    Util.time {
      val done = overlaps(input.tail, List(input.head), List())
      val d = done.flatMap(_.beacons).distinctBy(_.mkString)
      println(d.size)
    }
  }

  def parse(definition: String): Scanner = {
    val split = definition.split("\n")
    val number = split.head.split(" ")(2).toInt
    val beacons = split.tail.map(line => line.split(",").map(_.toInt)).toSet

    Scanner(number, beacons, Array(0, 0, 0), Identity)
  }

  def times(matrix: Array[Array[Int]], vec: Array[Int]): Array[Int] = {
    matrix.map(row => row.zip(vec).map(_ * _).sum)
  }

  def times(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    a.map(row => {
      b.transpose.map(col => {
        row.zip(col).map(_ * _).sum
      })
    })
  }

  def exp(matrix: Array[Array[Int]], exponent: Int): Array[Array[Int]] = exponent match {
    case 0 => Identity
    case 1 => matrix
    case _ => times(matrix, exp(matrix, exponent - 1))
  }

  def rel(scanner: Scanner, point: Array[Int]): Scanner = scanner match {
    case Scanner(idx, beacons, offset, rotation) => Scanner(idx, beacons.map(b => b.zip(point).map(_ - _)), offset, rotation)
  }

  def shift(scanner: Scanner, point: Array[Int]): Scanner = scanner match {
    case Scanner(idx, beacons, offset, rotation) => Scanner(idx, beacons.map(b => b.zip(point).map(_ + _)), offset.zip(point).map(_ + _), rotation)
  }

  def rotate(scanner: Scanner, rotation: Array[Array[Int]]): Scanner = scanner match {
    case Scanner(idx, beacons, offset, _) => Scanner(idx, beacons.map(b => times(rotation, b)), offset, rotation)
  }


  def findOverlap(pair: (Scanner, Scanner)): Option[Scanner] = {
    val tries = allRotations.flatMap(rot => {
      val s1rot = rotate(pair._2, rot)
      pair._1.beacons.toList.flatMap(b1 => {
        s1rot.beacons.toList.map(b2 => {
          (pair._1, s1rot, b1, b2)
        })
      })
    })

    tries.find({case (s0, s1, offset, r) =>
      shift(rel(s1, r), offset).beacons.map(_.mkString).intersect(s0.beacons.map(_.mkString)).size >= 12
    }).flatMap({case (s0, s1, offset, r) => Some(shift(rel(s1, r), offset))})
  }

  @tailrec
  def overlaps(scanners: List[Scanner], toMap: List[Scanner], mapped: List[Scanner]): List[Scanner] = toMap match {
    case x :: xs =>
      println(x.idx)
      val pairs = scanners.map(s => (x, s))
      val overlapping = pairs.map(findOverlap).filter(_.nonEmpty).map(_.get)
      val newS = scanners.filter(s => !overlapping.exists(o => o.idx == s.idx))
      val newToMap = xs.filter(s => !overlapping.exists(o => o.idx == s.idx)) ++ overlapping
      val newMapped = x :: mapped

      overlaps(newS, newToMap, newMapped)
    case Nil => mapped
  }

  /*Working but slow:
  def findOverlap(pair: (Scanner, Scanner)): Option[Scanner] = {
    val tries = allRotations.flatMap(rot => {
    pair._1.beacons.toList.map(b => {
      (pair._1, pair._2, rot, b)
    })})

    tries.map({ case (s0, s1, rot, offset) =>
      val allShifts = rotate(s1, rot).beacons.toList.map(b => {
        shift(rel(rotate(s1, rot), b), offset)
      })
      allShifts.find(scanner => scanner.beacons.map(_.mkString).intersect(s0.beacons.map(_.mkString)).size >= 12)
    }).find(_.nonEmpty).flatten
  }*/
}
