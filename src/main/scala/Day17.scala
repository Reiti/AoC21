import util.Util

import scala.annotation.tailrec

object Day17 {
  def main(args: Array[String]): Unit = {
    val targetArea = (207, 263, -115, -63)

    val hit = (1 until 2*targetArea._2).flatMap(xv => {
      (-targetArea._2 until targetArea._2).map(yv => {
        hits((0, 0), (xv, yv), targetArea, 0)
      })
    }).filter(_._1)

    //Part 1
    println(hit.maxBy(_._2)._2)

    //Part 2
    println(hit.size)
  }

  @tailrec
  def hits(pos: (Int, Int), v: (Int, Int), targetArea: (Int, Int, Int, Int), maxY: Int): (Boolean, Int) = {
    if(inside(pos, targetArea)) {
      (true, maxY)
    } else if(beyond(pos, v, targetArea)) {
      (false, maxY)
    } else {
      val nv = (if v._1 > 0 then v._1 - 1 else if v._1 < 0 then v._1 + 1 else 0, v._2 - 1)
      val np = (pos._1 + v._1, pos._2 + v._2)
      hits(np, nv, targetArea, if np._2 > maxY then np._2 else maxY)
    }
  }

  def beyond(pos: (Int, Int), v: (Int, Int), targetArea: (Int, Int, Int, Int)): Boolean = {
    pos._2 < targetArea._3 || (pos._1 < targetArea._1 && v._1 == 0) || (pos._1 > targetArea._2 && v._1 == 0)
  }

  def inside(pos: (Int, Int), targetArea: (Int, Int, Int, Int)): Boolean = {
    pos._1 >= targetArea._1 && pos._1 <= targetArea._2 && pos._2 >= targetArea._3 && pos._2 <= targetArea._4
  }
}
