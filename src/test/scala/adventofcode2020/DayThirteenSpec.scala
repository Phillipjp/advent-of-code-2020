package adventofcode2020

import adventofcode2020.DayThirteen.isFactor
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayThirteenSpec extends AnyFlatSpecLike with Matchers {

  it should "read in the estimated time and all bus ids that are in service" in {
    // Given
    val expected = (939, Seq("7", "13", "x", "x", "59", "x", "31", "19"))

    // When
    val actual = DayThirteen.readTimeTable("day-13-part-1-test.txt")

    // Then
    actual shouldBe expected
  }

  it should "find the earliest bus id and the time to get it" in {
    // Given, When
    val actual = DayThirteen.getEarliestBusIdAndTime(939, Seq(7,13,59,31,19))

    // Then
    actual shouldBe (59, 944)
  }

  it should "check if a number is a factor of another number" in {
    // Given, When, Then
    DayThirteen.isFactor(5, 20) shouldBe true
    DayThirteen.isFactor(3, 20) shouldBe false
  }

  it should "find the timestamp t" in {
    // Given
    val ids = Seq("7", "13", "x", "x", "59", "x", "31", "19")

    // When
    val actual = DayThirteen.findT(ids)

    // Then
    actual shouldBe 1068781
  }


  it should "chinese number theory" in {
    println(DayThirteen.chineseNumberTheory(Seq("7", "13", "x", "x", "59", "x", "31", "19")))
  }

}
