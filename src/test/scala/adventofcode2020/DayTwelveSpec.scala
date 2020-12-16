package adventofcode2020

import adventofcode2020.DayTwelve._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayTwelveSpec extends AnyFlatSpecLike with Matchers {

  it should "read in directions" in {
    // Given
    val expected = Seq(
      Direction(F, 10),
      Direction(N, 3),
      Direction(F, 7),
      Direction(R, 90),
      Direction(F, 11)
    )

    // When
    val actual = DayTwelve.readDirections("day-12-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "get the new heading given a current heading, a direction t tirn and the number of degrees to turn by" in {
    // Given, When, Then
    DayTwelve.getHeading(N, R, 90) shouldBe E
    DayTwelve.getHeading(N, R, 180) shouldBe S
    DayTwelve.getHeading(N, R, 270) shouldBe W
    DayTwelve.getHeading(E, R, 90) shouldBe S
    DayTwelve.getHeading(E, R, 180) shouldBe W
    DayTwelve.getHeading(E, R, 270) shouldBe N
    DayTwelve.getHeading(S, R, 90) shouldBe W
    DayTwelve.getHeading(S, R, 180) shouldBe N
    DayTwelve.getHeading(S, R, 270) shouldBe E
    DayTwelve.getHeading(W, R, 90) shouldBe N
    DayTwelve.getHeading(W, R, 180) shouldBe E
    DayTwelve.getHeading(W, R, 270) shouldBe S

    DayTwelve.getHeading(N, L, 90) shouldBe W
    DayTwelve.getHeading(N, L, 180) shouldBe S
    DayTwelve.getHeading(N, L, 270) shouldBe E
    DayTwelve.getHeading(E, L, 90) shouldBe N
    DayTwelve.getHeading(E, L, 180) shouldBe W
    DayTwelve.getHeading(E, L, 270) shouldBe S
    DayTwelve.getHeading(S, L, 90) shouldBe E
    DayTwelve.getHeading(S, L, 180) shouldBe N
    DayTwelve.getHeading(S, L, 270) shouldBe W
    DayTwelve.getHeading(W, L, 90) shouldBe S
    DayTwelve.getHeading(W, L, 180) shouldBe E
    DayTwelve.getHeading(W, L, 270) shouldBe N
  }

  it should "navigate the ship to completion" in {
    // Given
    val ship = Ship(E, 0, 0)
    val directions = Seq(
      Direction(F, 10),
      Direction(N, 3),
      Direction(F, 7),
      Direction(R, 90),
      Direction(F, 11)
    )

    // When
    val actual = DayTwelve.navigateShip(ship, directions)

    // Then
    actual shouldBe Ship(S, 17, -8)
  }

  it should "calculate the Manhattan distance of a ship from the origin" in {
    // Given, When, Then
    DayTwelve.calculateShipsManhattanDistance(Ship(S, 17, -8)) shouldBe 25
  }

  it should "rotate the waypoint" in {
    // Given
    val wayPoint = WayPoint(10, 4)
    // When, Then
    DayTwelve.rotateWayPoint(wayPoint, R, 90) shouldBe WayPoint(4, -10)
    DayTwelve.rotateWayPoint(wayPoint, L, 270) shouldBe WayPoint(4, -10)
    DayTwelve.rotateWayPoint(wayPoint, R, 180) shouldBe WayPoint(-10, -4)
    DayTwelve.rotateWayPoint(wayPoint, L, 180) shouldBe WayPoint(-10, -4)
    DayTwelve.rotateWayPoint(wayPoint, R, 270) shouldBe WayPoint(-4, 10)
    DayTwelve.rotateWayPoint(wayPoint, L, 90) shouldBe WayPoint(-4, 10)
  }

  it should "navigate the ship towards a waypoint" in {
    // Given
    val ship = Ship(E, 0, 0)
    val directions = Seq(
      Direction(F, 10),
      Direction(N, 3),
      Direction(F, 7),
      Direction(R, 90),
      Direction(F, 11)
    )

    // When
    val actual = DayTwelve.navigateShipToWaypoint(ship, WayPoint(10, 1), directions)

    // Then
    actual shouldBe Ship(E, 214, -72)
  }

}
