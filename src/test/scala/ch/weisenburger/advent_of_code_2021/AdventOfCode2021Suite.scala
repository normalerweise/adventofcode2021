package ch.weisenburger.advent_of_code_2021

import org.scalatest.funsuite.AnyFunSuite
import day1.SonarSweep
import day2.{Dive, Direction, Command}
import day3.BinaryDiagnostic
import day4.Bingo
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should._

class AdventOfCode2021Suite extends AnyWordSpec with Matchers {

  "Day 1 (Sonar Sweep) - Part 1" when {

    "executed with puzzle data" should {
      "return 1681 measurements larger than the previous measurement" in {
        SonarSweep.numberOfTimesDepthMeasurementsIncrease shouldBe 1681
      }
    }
  }

  "Day 1 (Sonar Sweep) - Part 2" when {

    "executed with puzzle data" should {
      "return 1704 sums larger than the previous sum" in {
        SonarSweep.numberOfTimesTheSumOfDepthMeasurementsInSlidingWindowsOf3Increases shouldBe 1704
      }
    }
  }

  "Day 2 (Dive) - Part 1" when {

    "executed with example data" should {
      "return 150 as multiple of final horizontal position and final depth" in {
        Dive.multipleOfFinalPositionAfterFollowingCourse(
          exampleCourse
        ) shouldBe 150
      }
    }

    "executed with puzzle data" should {
      "return 1714950 as multiple of final horizontal position and final depth" in {
        Dive.multipleOfFinalPositionAfterFollowingPlannedCourse shouldBe 1714950
      }
    }
  }

  "Day 2 (Dive) - Part 2" when {

    "executed with example data" should {
      "return 900 as multiple of final horizontal position and final depth using new interpretation of commands" in {
        Dive.multipleOfFinalPositionAfterFollowingCourseUsingeNewInstructions(
          exampleCourse
        ) shouldBe 900
      }
    }

    "executed with puzzle data" should {
      "return 1281977850 as multiple of final horizontal position and final depth using new interpretation of commands" in {
        Dive.multipleOfFinalPositionAfterFollowingPlannedCourseUsingeNewInstructions shouldBe 1281977850
      }
    }

  }

  "Day 3 (Binary Diagnostic) - Part 1" when {

    "executed with example data" should {
      "return 198 as the power consumption of the submarine" in {
        BinaryDiagnostic.powerConsumptionBasedOnReport(
          exampleDiagnosticReport
        ) shouldBe 198
      }
    }

    "executed with puzzle data" should {
      "return 3148794 as the power consumption of the submarine" in {
        BinaryDiagnostic.powerConsumptionBasedOnDiagnosticReport shouldBe 3148794
      }
    }
  }

  "Day 3 (Binary Diagnostic) - Part 2" when {

    "executed with example data" should {
      "return 230 as the life support rating of the submarine" in {
        BinaryDiagnostic.lifeSupportRatingBasedOnReport(
          exampleDiagnosticReport
        ) shouldBe 230
      }
    }

    "executed with puzzle data" should {
      "return 2795310 as the life support rating of the submarine" in {
        BinaryDiagnostic.lifeSupportRatingBasedOnDiagnosticReport shouldBe 2795310
      }
    }
  }

  "Day (Giant Squid) - Part 1" when {

    "executed with example data" should {
      "return 4512 as the Bingo Game final score" in {
        Bingo.parseAndPlayRawGame(exampleBingoGame) shouldBe 4512
      }
    }

    "executed with puzzle data" should {
      "return 14093 as the Bingo Game final score" in {
        Bingo.parseAndPlayRawGame(Bingo.readRawBingoGame()) shouldBe 14093
      }
    }
  }

  "Day (Giant Squid) - Part 2" when {

    "executed with example data" should {
      "return 1924 as the Bingo Game final score" in {
        Bingo.parseAndPlayRawGame(exampleBingoGame, Bingo.Game.whichBoardHasBingoLast) shouldBe 1924
      }
    }

    "executed with puzzle data" should {
      "return 17388 as the Bingo Game final score" in {
        Bingo.parseAndPlayRawGame(Bingo.readRawBingoGame(), Bingo.Game.whichBoardHasBingoLast) shouldBe 17388
      }
    }

  }

  private def exampleDiagnosticReport =
    Seq(
      "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    ).iterator

  private def exampleCourse =
    import Direction.*
    Seq(
      Command(Forward, 5),
      Command(Down, 5),
      Command(Forward, 8),
      Command(Up, 3),
      Command(Down, 8),
      Command(Forward, 2)
    ).iterator

  private def exampleBingoGame =
    Seq(
      "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
      "",
      "22 13 17 11  0",
      " 8  2 23  4 24",
      "21  9 14 16  7",
      " 6 10  3 18  5",
      " 1 12 20 15 19",
      "",
      " 3 15  0  2 22",
      " 9 18 13 17  5",
      "19  8  7 25 23",
      "20 11 10 24  4",
      "14 21 16 12  6",
      "",
      "14 21 17 24  4",
      "10 16 15  9 19",
      "18  8 23 26 20",
      "22 11 13  6  5",
      " 2  0 12  3  7"
    )
}
