package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object DayTwelve {

  sealed trait Action
  sealed trait Heading
  sealed trait Turn

  case object N extends Action with Heading
  case object S extends Action with Heading
  case object E extends Action with Heading
  case object W extends Action with Heading
  case object L extends Action with Turn
  case object R extends Action with Turn
  case object F extends Action

  case class Ship(heading: Heading, x: Int, y: Int)

  case class Direction(action: Action, value: Int)

  case class WayPoint(x: Int, y: Int)

  def readDirections(name: String): Seq[Direction] = {
    Source.fromResource(name)
      .getLines()
      .map{row =>
        val action = row.charAt(0) match {
          case 'N' => N
          case 'S' => S
          case 'E' => E
          case 'W' => W
          case 'L' => L
          case 'R' => R
          case 'F' => F
        }
        Direction(action, row.drop(1).toInt)
      }
      .toSeq
  }

  private val headingToDegrees: Map[Heading, Int] = Map(N -> 0, E -> 90, S -> 180, W -> 270)
  private val degreesToHeading: Map[Int, Heading] = Map(0 -> N, 90 -> E, 180 -> S, 270 -> W)

  def getHeading(shipHeading: Heading, turn: Turn, degrees: Int): Heading = {
    val signedDegrees = turn match{
      case L => degrees * -1
      case R => degrees
    }
    val newHeadingInDegrees = headingToDegrees(shipHeading) + signedDegrees

    if(newHeadingInDegrees >= 360){
      degreesToHeading(newHeadingInDegrees - 360)
    }
    else if(newHeadingInDegrees < 0){
      degreesToHeading(newHeadingInDegrees + 360)
    }
    else {
      degreesToHeading(newHeadingInDegrees)
    }


  }
  def navigateShip(ship: Ship, directions: Seq[Direction]): Ship = {

    @tailrec
    def navigate(ship: Ship, directions: Seq[Direction]): Ship = {
      if(directions.isEmpty){
        ship
      }
      else{
        val direction = directions.head
        val movedShip = direction.action match {
          case N => ship.copy(y = ship.y + direction.value)
          case S => ship.copy(y = ship.y - direction.value)
          case E => ship.copy(x = ship.x + direction.value)
          case W => ship.copy(x = ship.x - direction.value)
          case L => ship.copy(heading = getHeading(ship.heading, L, direction.value))
          case R => ship.copy(heading = getHeading(ship.heading, R, direction.value))
          case F =>
            ship.heading match {
              case N => ship.copy(y = ship.y + direction.value)
              case S => ship.copy(y = ship.y - direction.value)
              case E => ship.copy(x = ship.x + direction.value)
              case W => ship.copy(x = ship.x - direction.value)
            }
        }
        navigate(movedShip, directions.tail)
      }
    }

    navigate(ship, directions)

  }

  def rotateWayPoint(wayPoint: WayPoint, turn: Turn, degrees: Int): WayPoint = {
    if(degrees == 180){
      wayPoint.copy(x = wayPoint.x * -1, y = wayPoint.y * -1)
    }
    else if(turn == R && degrees == 90 || turn == L && degrees == 270){
      wayPoint.copy(x = wayPoint.y, y = wayPoint.x * -1)
    }
    else{
      wayPoint.copy(x = wayPoint.y * -1, y = wayPoint.x)
    }
  }

  def navigateShipToWaypoint(ship: Ship, waypoint: WayPoint, directions: Seq[Direction]): Ship = {

    @tailrec
    def navigate(ship: Ship, waypoint: WayPoint, directions: Seq[Direction]): Ship = {
      if(directions.isEmpty){
        ship
      }
      else{
        val direction = directions.head
        val (movedShip, movedWayPoint) = direction.action match {
          case N => (ship, waypoint.copy(y = waypoint.y + direction.value))
          case S => (ship, waypoint.copy(y = waypoint.y - direction.value))
          case E => (ship, waypoint.copy(x = waypoint.x + direction.value))
          case W => (ship, waypoint.copy(x = waypoint.x - direction.value))
          case L => (ship, rotateWayPoint(waypoint, L, direction.value))
          case R => (ship, rotateWayPoint(waypoint, R, direction.value))
          case F => (ship.copy(x = ship.x + waypoint.x * direction.value, y = ship.y + waypoint.y * direction.value), waypoint)
        }
        navigate(movedShip, movedWayPoint, directions.tail)
      }
    }

    navigate(ship, waypoint, directions)
  }

  def calculateShipsManhattanDistance(ship: Ship): Int = {
    Math.abs(ship.x) + Math.abs(ship.y)
  }

  def main(args: Array[String]): Unit = {
    val directions = readDirections("day-12-part-1.txt")
    val finalShipPartOne = navigateShip(Ship(E, 0, 0), directions)
    val manhattanDistancePartOne = calculateShipsManhattanDistance(finalShipPartOne)

    println(s"Part One: $manhattanDistancePartOne")

    val finalShipPartTwo = navigateShipToWaypoint(Ship(E, 0, 0), WayPoint(10, 1), directions)
    val manhattanDistancePartTwo = calculateShipsManhattanDistance(finalShipPartTwo)

    println(s"Part One: $manhattanDistancePartTwo")
  }
}
