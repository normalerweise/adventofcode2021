package ch.weisenburger.advent_of_code_2021

import org.scalatest.funsuite.AnyFunSuite
import day1.SonarSweep
import day2.{Dive, Direction, Command}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should._
import ch.weisenburger.advent_of_code_2021.day3.BinaryDiagnostic

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
      "return as the power consumption of the submarine" in {
        BinaryDiagnostic.powerConsumptionBasedOnDiagnosticReport shouldBe 1
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

}
