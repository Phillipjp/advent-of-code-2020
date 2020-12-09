package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object DayNine {

  def readInput(name: String): Seq[Long] = {
    Source.fromResource(name)
      .getLines()
      .map(_.toLong)
      .toSeq
  }

  def partOne(input: Seq[Long], preambleSize: Int): Long = {

    @tailrec
    def findExceptionToRule(input: Seq[Long], index: Int, preambleSize: Int): Long = {
      val number = input(index)
      val meetsRule = input.slice(index - preambleSize, index).combinations(2).map(_.sum).contains(number)
      if(meetsRule){
        findExceptionToRule(input, index + 1, preambleSize)
      }
      else{
        number
      }
    }

    findExceptionToRule(input, preambleSize, preambleSize)
  }

  def partTwo(input: Seq[Long], invalidNumber: Long): Long = {

    @tailrec
    def findSequenceThatSumsToInvalidNumber(input: Seq[Long], windowSize: Int, invalidNumber: Long): Seq[Long] = {
      val sumSequence = input.sliding(windowSize, 1).find(s => s.sum == invalidNumber)
      sumSequence match {
        case Some(sequence) => sequence
        case None => findSequenceThatSumsToInvalidNumber(input, windowSize + 1, invalidNumber)
      }
    }
    val sumSequence = findSequenceThatSumsToInvalidNumber(input, 2, invalidNumber)

    sumSequence.min + sumSequence.max
  }


  def main(args: Array[String]): Unit = {
    val input = readInput("day-9-part-1.txt")
    val answerOne = partOne(input, 25)
    println(s"Part One: $answerOne")

    val answerTwo = partTwo(input, answerOne)
    println(s"Part Two: $answerTwo")


  }

}
