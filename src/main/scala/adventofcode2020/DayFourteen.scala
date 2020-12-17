package adventofcode2020

import scala.io.Source

object DayFourteen {

  def runProgramWithDecoderOne(program: Seq[String]): Long = {

    def run(program: Seq[String], memory: Map[String, Long], mask: String): Map[String, Long] ={
      if(program.isEmpty){
        memory
      }
      else{
        val line = program.head
        val maskPattern = "mask = (.*)".r
        val memoryPattern = "mem(.*)".r
        val pattern = "(\\[\\d+\\]) (=) (\\d+)".r
        line match {
          case maskPattern(newMask) => run(program.tail, memory, newMask)
          case memoryPattern(memStr) =>
            val  pattern(memoryAddress, _, value) = memStr

            val maskedValue = intToBinaryString(value.toInt, 36)
              .zip(mask)
              .map{case(v, m) => if(m == 'X') v else m}
              .mkString

            val longValue = binaryStringToLong(maskedValue)

            val updatedMemory = memory + (memoryAddress -> longValue)
            run(program.tail, updatedMemory, mask)
        }

      }
    }

    run(program, Map(), "").values.sum
  }

  def intToBinaryString(int: Int, bits: Int): String = {

    def toBinary(quotient: Int, remainders: Seq[Int]): Seq[Int] = {
      val newQuotient = quotient / 2
      if(newQuotient == 0){
        remainders :+ (quotient % 2)
      }
      else{
        toBinary(newQuotient, remainders :+ (quotient % 2))
      }

    }
    toBinary(int, Seq()).mkString.padTo(bits, '0').reverse
  }

  def binaryStringToLong(binary: String): Long = {
    binary
      .reverse
      .zipWithIndex
      .map{case(v, i) =>
        if(v == '0'){
          0
        }
        else{
          Math.pow(2, i).toLong
        }
      }
      .sum
  }

  def runProgramWithDecoderTwo(program: Seq[String]): Long = {

    def run(program: Seq[String], memory: Map[String, Long], mask: String): Map[String, Long] ={
      if(program.isEmpty){
        memory
      }
      else{
        val line = program.head
        val maskPattern = "mask = (.*)".r
        val memoryPattern = "mem(.*)".r
        val pattern = "(\\[\\d+\\]) (=) (\\d+)".r
        line match {
          case maskPattern(newMask) => run(program.tail, memory, newMask)
          case memoryPattern(memStr) =>
            val  pattern(memoryAddress, _, value) = memStr

            val maskedAddresses = getMemoryAddresses(intToBinaryString(memoryAddress.drop(1).dropRight(1).toInt, 36), mask)

            val newMemoryValues = maskedAddresses.map(address => address -> value.toLong).toMap
            val updatedMemory = memory ++ newMemoryValues
            run(program.tail, updatedMemory, mask)
        }

      }
    }

    run(program, Map(), "").values.sum
  }

  def getMemoryAddresses(binary: String, mask: String): Seq[String] = {

    def applyMask(binary: String, mask: String, i: Int): Seq[String] = {
      if(i >= mask.length){
        Seq(binary)
      }
      else{
        mask.charAt(i) match {
          case '0' => applyMask(binary, mask, i + 1)
          case '1' =>
            val newBinary = binary.substring(0, i) + "1" + binary.substring( i + 1)
            applyMask(newBinary, mask, i + 1)
          case 'X' =>
              Seq(applyMask(binary.substring(0, i) + "1" + binary.substring( i + 1), mask, i + 1),
              applyMask(binary.substring(0, i) + "0" + binary.substring( i + 1), mask, i + 1)).flatten
        }
      }
    }

    applyMask(binary, mask, 0)
  }

  def main(args: Array[String]): Unit = {
    val program = Source.fromResource("day-14-part-1.txt").getLines().toSeq

    val partOne = runProgramWithDecoderOne(program)
    println(s"Part One: $partOne")

    val partTwo = runProgramWithDecoderTwo(program)
    println(s"Part Two: $partTwo")
  }
}
