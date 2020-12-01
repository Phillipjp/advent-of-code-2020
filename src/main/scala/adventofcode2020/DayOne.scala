package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object DayOne {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day-1-part-1.txt").getLines().toSeq.map(_.toInt)

    val partOneAnswer = recursiveProductOfNumbersThatAddTo2020(input, 2)
    val partTwoAnswer = recursiveProductOfNumbersThatAddTo2020(input, 3)

    println(s"Part One: $partOneAnswer")
    println(s"Part Two: $partTwoAnswer")
  }

  def productOfNumbersThatAddTo2020(input: Seq[Int], n: Int): Int = {
    input.combinations(n).filter(l => l.sum == 2020).toSeq.head.product
  }

  def recursiveProductOfNumbersThatAddTo2020(input: Seq[Int], n: Int): Int = {
    val combinations = input.combinations(n).toSeq

    @tailrec
    def productOfNumbersThatAddTo2020(combinations: Seq[Seq[Int]]): Int = {
      val head = combinations.head
      if(head.sum == 2020){
        head.product
      }
      else{
        productOfNumbersThatAddTo2020(combinations.tail)
      }
    }

    productOfNumbersThatAddTo2020(combinations)

  }

}
