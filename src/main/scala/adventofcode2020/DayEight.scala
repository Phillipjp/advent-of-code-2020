package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object DayEight {

  sealed trait Operation

  case object Accumulate extends Operation
  case object Jump extends Operation
  case object NoOperation extends Operation

  private val stringToOperation = Map("acc" -> Accumulate, "jmp" -> Jump, "nop" -> NoOperation)

  case class Instruction(operation: Operation, argument: Int, visited: Boolean)

  def readInstructions(name: String): Seq[Instruction] = {
    Source.fromResource(name)
      .getLines()
      .map{line =>
        val separated = line.split(" ")
        val argument = separated.last.head match{
          case s if s == '+'=> separated.last.tail.toInt
          case s if s == '-'=> - separated.last.tail.toInt
        }
        Instruction(stringToOperation(separated.head), argument, false)
      }
      .toSeq
  }

  def findAccumulatorValue(instructions: Seq[Instruction]): Int = {

    @tailrec
    def runInstructions(instructions: Seq[Instruction], index: Int, accumulator: Int): Int = {
      if(index >= instructions.size){
        accumulator
      }
      else if(instructions(index).visited){
        accumulator
      }
      else{
        val instruction = instructions(index)
        val (newIndex, updatedAccumulator) = instruction.operation match {
          case Accumulate => (index + 1, accumulator + instruction.argument)
          case Jump => (index + instruction.argument, accumulator)
          case NoOperation => (index + 1, accumulator)
        }

        val updatedInstructions = instructions.updated(index, instruction.copy(visited = true))

        runInstructions(updatedInstructions, newIndex, updatedAccumulator)
      }
    }

    runInstructions(instructions, 0, 0)

  }

  def fixInstructions(instructions: Seq[Instruction], brokenRoute: Seq[Int]): Seq[Instruction] = {
    val possibleInstructions = Stream.continually(instructions)
      .take(brokenRoute.length)
      .zip(brokenRoute)
      .map{case (instructions, index) =>
         val updatedInstruction = instructions(index).operation match{
           case Accumulate => instructions(index)
           case Jump => instructions(index).copy(operation = NoOperation)
           case NoOperation => instructions(index).copy(operation = Jump)
         }
        instructions.updated(index, updatedInstruction)
      }

    @tailrec
    def isValidInstructions(instructions: Seq[Instruction], index: Int): Boolean = {
      if(index >= instructions.size){
        true
      }
      else if(instructions(index).visited){
        false
      }
      else{
        val instruction = instructions(index)
        val newIndex = instruction.operation match {
          case Accumulate => index + 1
          case Jump => index + instruction.argument
          case NoOperation => index + 1
        }
        val updatedInstructions = instructions.updated(index, instruction.copy(visited = true))
        isValidInstructions(updatedInstructions, newIndex)
      }
    }
    possibleInstructions.find(i => isValidInstructions(i, 0)).get
  }

  private [adventofcode2020] def getBrokenRoute(instructions: Seq[Instruction]): Seq[Int] = {

    def traverseInstructions(instructions: Seq[Instruction], route: Seq[Int]): Seq[Int] = {
      val index = route.last
      val instruction = instructions(index)
      if (instruction.visited) {
        route
      }
      else {
        val newIndex = instruction.operation match {
          case Accumulate => index + 1
          case Jump => index + instruction.argument
          case NoOperation => index + 1
        }

        val updatedInstructions = instructions.updated(index, instruction.copy(visited = true))

        traverseInstructions(updatedInstructions, route :+ newIndex)
      }
    }
    traverseInstructions(instructions, Seq(0))
  }

  def main(args: Array[String]): Unit = {
    val instructions = readInstructions("day-8-part-1.txt")
    val accumulatorValue = findAccumulatorValue(instructions)

    println(s"Part One: $accumulatorValue")

    val brokenRoute = getBrokenRoute(instructions)
    val fixedInstructions = fixInstructions(instructions, brokenRoute)
    val fixedAccumulatorValue = findAccumulatorValue(fixedInstructions)
    println(s"Part Two: $fixedAccumulatorValue")
  }

}
