package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object DaySeven {

  case class InnerBag(name: String, number: Int)

  private val EMPTY_BAG = InnerBag("no other bags", 0)

  def readBaggageRules(name: String): Map[String, Seq[InnerBag]] = {
    Source.fromResource(name)
      .getLines()
      .map { line =>
        val separated = line.split("contain")
        val name = separated.head
        val innerBagsStr = separated.last
        val numPattern = "[0-9]+".r
        val namePattern = "[a-z]+".r

        val innerBags = numPattern.findFirstMatchIn(innerBagsStr) match{
          case Some(_) =>
            innerBagsStr
              .dropRight(1)
              .split(",")
              .map{innerBag =>
                val number = numPattern.findFirstMatchIn(innerBag).get.toString().toInt
                val n = namePattern.findAllIn(innerBag).mkString(" ").replaceAll(" bags", " bag")
                InnerBag(n, number)
              }.toSeq
          case None => Seq(EMPTY_BAG)
        }
        (name.dropRight(2), innerBags)
      }
      .toMap
  }

  def findWhatBagsContainGivenBag(bag: String, rules: Seq[(String, Seq[InnerBag])]): Seq[String] = {


    @tailrec
    def findBags(innerBagNames: Seq[String], outerBagNames: Seq[String]): Seq[String] = {
      if(innerBagNames.isEmpty){
        outerBagNames
      }
      else{
        val newOuterBags = innerBagNames.flatMap{innerBag =>
          rules.filter{case (_, innerBags) => innerBags.map(_.name).contains(innerBag)}.map{case (name, _) => name}
        }.distinct

        findBags(newOuterBags, outerBagNames ++ newOuterBags)

      }
    }

    findBags(Seq(bag), Seq()).distinct

  }

  def findNeededNumberOfBags(bag: String, rules: Map[String, Seq[InnerBag]]): Int = {

    @tailrec
    def findNumberOfBags(outerBagNames: Seq[String], total: Int): Int = {
      if(outerBagNames.isEmpty){
        total
      }
      else{
        val innerBags = outerBagNames.flatMap(name =>rules(name))
        val newOuterBagNames = innerBags.filter(bag => bag != EMPTY_BAG).flatMap(bag => Stream.continually(bag.name).take(bag.number))

        findNumberOfBags(newOuterBagNames, total + innerBags.map(_.number).sum)
      }
    }
    findNumberOfBags(Seq(bag), 0)

  }

  def main(args: Array[String]): Unit = {
    val rules = readBaggageRules("day-7-part-1.txt")
    val bags = findWhatBagsContainGivenBag("shiny gold bag", rules.toSeq).size

    println(s"Part One: $bags")

    val bagsNeeded = findNeededNumberOfBags("shiny gold bag", rules)
    println(s"Part Two: $bagsNeeded")
  }

}
