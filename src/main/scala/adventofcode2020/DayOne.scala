package adventofcode2020

import scala.io.Source

object DayOne {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day-1-part-1.txt").getLines().toSeq.map(_.toInt)

    val partOneAnswer = productOfNumbersThatAddTo2020(input, 2)
    val partTwoAnswer = productOfNumbersThatAddTo2020(input, 3)

    println(s"Part One: $partOneAnswer")
    println(s"Part Two: $partTwoAnswer")
  }

  def productOfNumbersThatAddTo2020(input: Seq[Int], n: Int): Int = {
    input.combinations(n).filter(l => l.sum == 2020).toSeq.head.product
  }

}
