package adventofcode2020

import scala.io.Source

object DayFive {

  case class SeatIdentifier(row: String, column: String){

    def getSeatID: Int = {
      getRowNumber * 8 + getColumnNumber
    }

    private [adventofcode2020] def getRowNumber: Int = {
      val binary = identifierToBinaryString(row, 'F', 'B')
      Integer.parseInt(binary, 2)
    }

    private [adventofcode2020] def getColumnNumber: Int ={
      val binary = identifierToBinaryString(column, 'L', 'R')
      Integer.parseInt(binary, 2)
    }

    private def identifierToBinaryString(identifier: String, zeroValue: Char, oneValue: Char): String = {
      identifier.map {
        case z if z == zeroValue => '0'
        case o if o == oneValue => '1'
      }
    }
  }

  def readSeatIdentifiers(name: String): Seq[SeatIdentifier] = {
    Source.fromResource(name)
      .getLines()
      .map(line => SeatIdentifier(line.substring(0,7), line.substring(7,line.length)))
      .toSeq
  }

  def findHighestSeatID(seatIdentifiers: Seq[SeatIdentifier]): Int = {
    seatIdentifiers.map(_.getSeatID).max
  }

  def findSeat(seatIdentifiers: Seq[SeatIdentifier]): Int = {
    seatIdentifiers.map(_.getSeatID)
      .sorted
      .sliding(3, 1)
      .find(tuple => tuple.head + 1 != tuple(1) && tuple.head + 2 != tuple.last)
      .get
      .head + 1
  }

  def main(args: Array[String]): Unit = {
    val seatIdentifiers = readSeatIdentifiers("day-5-part-1.txt")

    val highestSeatID = findHighestSeatID(seatIdentifiers)

    println(s"Part One: $highestSeatID")

    val seat = findSeat(seatIdentifiers)

    println(s"Part Two: $seat")


  }


}
