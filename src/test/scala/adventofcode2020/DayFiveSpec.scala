package adventofcode2020

import adventofcode2020.DayFive.SeatIdentifier
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayFiveSpec extends AnyFlatSpecLike with Matchers {

  it should "read in seat identifiers" in {
    // Given
    val expected = Seq(
      SeatIdentifier("BFFFBBF", "RRR"),
      SeatIdentifier("FFFBBBF", "RRR"),
      SeatIdentifier("BBFFBBF", "RLL")
    )

    // When
    val actual = DayFive.readSeatIdentifiers("day-5-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "get the row number as an integer" in {
    // Given, When, Then
    SeatIdentifier("BFFFBBF", "RRR").getRowNumber shouldBe 70
  }

  it should "get the column number as an integer" in {
    // Given, When, Then
    SeatIdentifier("BFFFBBF", "RRR").getColumnNumber shouldBe 7
  }

  it should "get the seat ID" in {
    // Given, When, Then
    SeatIdentifier("BFFFBBF", "RRR").getSeatID shouldBe 567
  }

  it should "get the highest seat ID" in {
    // Given
    val seatIdentifiers = Seq(
      SeatIdentifier("BFFFBBF", "RRR"),
      SeatIdentifier("FFFBBBF", "RRR"),
      SeatIdentifier("BBFFBBF", "RLL")
    )

    // When
    val actual = DayFive.findHighestSeatID(seatIdentifiers)

    // Then
    actual shouldBe 820

  }

}
