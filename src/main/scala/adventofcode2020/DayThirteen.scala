package adventofcode2020

import scala.io.Source

object DayThirteen {

  def readTimeTable(name: String): (Int, Seq[String]) = {
    val lines = Source.fromResource(name)
      .getLines()
      .toSeq

    val ids = lines.last.split(",")
    (lines.head.toInt, ids)
  }

  def getEarliestBusIdAndTime(estimate: Int, ids: Seq[Int]): (Int, Int) = {

    val trup = ids.map{id =>
      (id, ((Math.ceil(estimate / id) + 1) * id).toInt, (Math.ceil(estimate / id) * id).toInt - estimate)
    }.sortWith(_._3 < _._3).head

    (trup._1, trup._2)

  }

  def isFactor(a: Int, b: Long): Boolean = b % a == 0

  def findT(ids: Seq[String]): Long = {
    val idsWithIndex = ids.zipWithIndex.filter(_._1 != "x").map(i => (i._1.toInt, i._2))

    val start = idsWithIndex.head._1.toLong
    Stream.iterate(start)(_ + start)
      .takeWhile{t =>
        !idsWithIndex.forall{ case(id, index) =>
          isFactor(id, t + index)
        }
      }.last + start
  }

  def chineseNumberTheory(ids: Seq[String]): Long = {
    val idsWithIndex = ids.zipWithIndex.filter(_._1 != "x").map(i => (i._1.toInt, i._2)).tail

    val x = Stream.continually(idsWithIndex).take(idsWithIndex.length).zipWithIndex.map{case(ids, i)=>
      val bi = i
      val Ni = ids.filter(_._2 != i).map(_._1).product
      val ni = ids(i)._1
      val c = Ni % ni
      val xi = if(c == 1 || c == 0) {
        1
      }
      else{
        Stream.iterate(1)(_ + 1).takeWhile(n => c % n == 1).last + 1
      }

      bi*Ni*xi
    }.sum

    val N = idsWithIndex.map(_._1).filter(_ != 0).product
    x / N
  }

  def main(args: Array[String]): Unit = {
    val (estimate, ids) = readTimeTable("day-13-part-1.txt")
    val validIds = ids.filter(_ != "x").map(_.toInt)
    val (earliestBusID, time) = getEarliestBusIdAndTime(estimate, validIds)

    val answerOne = (time - estimate) * earliestBusID

    println(s"Part One: $answerOne")

    val t = findT(ids)
    println(s"Part Two: $t")
  }

}
