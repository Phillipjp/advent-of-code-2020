package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object DayThree {

  case class Slope(right: Int, down: Int)

  def readMap(name: String): Seq[Seq[Char]] = {
    Source.fromResource(name)
      .getLines()
      .map(line => line.toSeq)
      .toSeq
  }

  def countTreesTraveledPast(map: Seq[Seq[Char]], slope: Slope): Int = {


    @tailrec
    def countTrees(map: Seq[Seq[Char]], slope: Slope, trees: Int, x: Int, y: Int): Int = {
      if(y + slope.down > map.length - 1){
        trees
      }
      else{

        val newX =
          if(x + slope.right > map(y).length - 1){
            x + slope.right - map(y).length
          }
          else{
            x + slope.right
          }

        val newY = y + slope.down

        if(map(newY)(newX) == '#'){
          countTrees(map, slope, trees+1, newX, newY)
        }
        else{
          countTrees(map, slope, trees, newX, newY)
        }
      }
    }

    countTrees(map, slope, 0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    val map = readMap("day-3-part-1.txt")

    val trees = countTreesTraveledPast(map, Slope(3,1))
    println(s"Part One: $trees")

    val slopes = Seq(Slope(1,1), Slope(3, 1), Slope(5, 1), Slope(7, 1), Slope(1, 2))
    val productOfTrees = slopes.map(slope => countTreesTraveledPast(map, slope).toLong).product

    println(s"Part Two: $productOfTrees")


  }
}
