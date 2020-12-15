package adventofcode2020

import scala.io.Source

object DayEleven {

  def readSeatingMap(name: String): Map[(Int, Int), Char] = {
    Source.fromResource(name)
      .getLines()
      .zipWithIndex
      .flatMap{case(row, y) =>
        row.zipWithIndex.map{case (s, x) => (x,y) -> s}
      }
      .toMap
  }

  def getAdjacentOccupiedSeats()(seatingMap: Map[(Int, Int), Char], seat: (Int, Int)): Int = {
    val start = (seat._1 - 1, seat._2 - 1)
    val seats = Stream.iterate(start)(s => nextSeat(s, start._2 + 2, start._2)).takeWhile(_._1 < start._1 + 3).filter(_ != seat).toList
    seats.map(s => seatingMap.getOrElse(s, '_')).filter(_ != '_').count(s => s =='#')
  }

  private def nextSeat(current: (Int, Int), size: Int, xReset: Int): (Int, Int) = {
    if (current._2 < size)
      (current._1, current._2 + 1)
    else
      (current._1 + 1, xReset)
  }

  def getNumberOfOccupiedSeats(seatingMap:  Map[(Int, Int), Char], getOccupiedSeats: (Map[(Int, Int), Char], (Int, Int)) => Int, n: Int): Int = {

    def round(seatingMap:  Map[(Int, Int), Char]):  Map[(Int, Int), Char] = {

      val updatedSeatingMap = seatingMap.keys.map{seat =>
        val occupiedSeats = getOccupiedSeats(seatingMap, seat)
        if(occupiedSeats == 0 && seatingMap(seat) == 'L'){
          seat -> '#'
        }
        else if (occupiedSeats >= n && seatingMap(seat) == '#'){
          seat -> 'L'
        }
        else{
          seat -> seatingMap(seat)
        }
      }.toMap


      if(updatedSeatingMap == seatingMap){
        seatingMap
      }
      else{
        round(updatedSeatingMap)
      }
    }

    round(seatingMap).values.count(s => s =='#')
  }

  def getVisibleOccupiedSeats()(seatingMap: Map[(Int, Int), Char], seat: (Int, Int)): Int = {
    val nextSeats: Seq[((Int, Int)) => (Int, Int)] = Seq(nextNorthSeat(), nextSouthSeat(), nextEastSeat(), nextWestSeat(), nextNorthEastSeat(), nextSouthEastSeat(), nextNorthWestSeat(), nextSouthWestSeat())
    Stream.continually(seat).take(8).zip(nextSeats).map{case (s, ns) => nextVisibleSeat(seatingMap, s, ns)}.filter(_ != '_').count(_ == '#')
  }

  def nextNorthSeat()(seat: (Int, Int)): (Int, Int) =  (seat._1, seat._2 + 1)

  def nextSouthSeat()(seat: (Int, Int)): (Int, Int) =  (seat._1, seat._2 - 1)

  def nextEastSeat()(seat: (Int, Int)): (Int, Int) =  (seat._1 + 1, seat._2)

  def nextWestSeat()(seat: (Int, Int)): (Int, Int) = (seat._1 - 1, seat._2)

  def nextNorthEastSeat()(seat: (Int, Int)): (Int, Int) = (seat._1 + 1, seat._2 + 1)

  def nextSouthEastSeat()(seat: (Int, Int)): (Int, Int) = (seat._1 + 1, seat._2 - 1)

  def nextNorthWestSeat()(seat: (Int, Int)): (Int, Int) = (seat._1 - 1, seat._2 + 1)

  def nextSouthWestSeat()(seat: (Int, Int)): (Int, Int) = (seat._1 - 1, seat._2 - 1)


  def nextVisibleSeat(seatingMap: Map[(Int, Int), Char], seat: (Int, Int), nextSeat: ((Int, Int)) => (Int, Int)): Char = {
    val newSeat = nextSeat(seat)
    val seatValue = seatingMap.getOrElse(newSeat, '_')
    if(seatValue != '.'){
      seatValue
    }
    else{
      nextVisibleSeat(seatingMap, newSeat, nextSeat)
    }

  }

  def main(args: Array[String]): Unit = {
    val seatingMap = readSeatingMap("day-11-part-1.txt")
    val occupiedSeats = getNumberOfOccupiedSeats(seatingMap, getAdjacentOccupiedSeats(), 4)
    println(s"Part One: $occupiedSeats")

    val occupiedSeats2 = getNumberOfOccupiedSeats(seatingMap, getVisibleOccupiedSeats(), 5)
    println(s"Part Two: $occupiedSeats2")
  }



}
