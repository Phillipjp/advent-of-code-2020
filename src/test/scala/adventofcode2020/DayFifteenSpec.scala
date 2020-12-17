package adventofcode2020

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DayFifteenSpec extends AnyFlatSpecLike with Matchers{

  it should "get the 2020th number said" in {
    // Given, When, Then
    DayFifteen.playMemoryGame(Seq(0,3,6), 2020) shouldBe 436
    DayFifteen.playMemoryGame(Seq(1,3,2), 2020) shouldBe 1
    DayFifteen.playMemoryGame(Seq(2,1,3), 2020) shouldBe 10
    DayFifteen.playMemoryGame(Seq(1,2,3), 2020) shouldBe 27
  }

  it should "get the 30000000th number said" in {
    // Given, When, Then
    DayFifteen.playMemoryGame(Seq(0,3,6), 30000000) shouldBe 175594
    DayFifteen.playMemoryGame(Seq(1,3,2), 30000000) shouldBe 2578
    DayFifteen.playMemoryGame(Seq(2,1,3), 30000000) shouldBe 3544142
    DayFifteen.playMemoryGame(Seq(1,2,3), 30000000) shouldBe 261214
  }

}
