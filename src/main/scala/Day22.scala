import util.Util

import scala.annotation.tailrec

object Day22:
  case class Cuboid(on: Boolean, x1: Long, x2: Long, y1: Long, y2: Long, z1: Long, z2: Long)

  def main(args: Array[String]): Unit =
    val cuboids = Util.loadDayLines(22).map(parse)

    //Part 1
    println(solve(p1Restricted(cuboids), List()))

    //Part 2
    println(solve(cuboids, List()))

  def parse(cuboid: String): Cuboid =
    val s = cuboid.split(" ")
    val v = s(1).split(",")
    val x = v(0).split("=")(1).split("\\.\\.")
    val y = v(1).split("=")(1).split("\\.\\.")
    val z = v(2).split("=")(1).split("\\.\\.")
    Cuboid(s(0).equals("on"), x(0).toLong, x(1).toLong, y(0).toLong, y(1).toLong, z(0).toLong, z(1).toLong)

  def valid(c: Cuboid): Boolean =
    c.x1 <= c.x2 && c.y1 <= c.y2 && c.z1 <= c.z2

  def volume(c: Cuboid): Long =
    (c.x2 - c.x1 + 1) * (c.y2 - c.y1 + 1) * (c.z2 - c.z1 + 1)

  @tailrec
  def solve(cuboids: List[Cuboid], space: List[Cuboid]): Long = cuboids match
    case x :: xs =>
      val i = intersections(x, space)
      val n = if x.on then x :: i else i
      solve(xs, space ++ n)
    case _ => space.map(c => if c.on then volume(c) else -volume(c)).sum

  def intersections(c: Cuboid, space: List[Cuboid]): List[Cuboid] =
    space.map(c1 => intersection(c, c1)).filter(valid)

  def intersection(c1: Cuboid, c2: Cuboid): Cuboid =
    Cuboid(
      !c2.on,
      Math.max(c1.x1, c2.x1), Math.min(c1.x2, c2.x2),
      Math.max(c1.y1, c2.y1), Math.min(c1.y2, c2.y2),
      Math.max(c1.z1, c2.z1), Math.min(c1.z2, c2.z2)
    )

  def p1Restricted(cuboids: List[Cuboid]): List[Cuboid] =
    cuboids.map(c => c.copy(
      x1 = Math.max(c.x1, -50), x2 = Math.min(c.x2, 50),
      y1 = Math.max(c.y1, -50), y2 = Math.min(c.y2, 50),
      z1 = Math.max(c.z1, -50), z2 = Math.min(c.z2, 50)
    )).filter(valid)