package ch.weisenburger.advent_of_code_2021.day1

import scala.io.Source
import SonarReportProcessor.determineNoOfDepthIncreases
import ch.weisenburger.advent_of_code_2021.util.Result

type SonarSweepReport = Iterator[Int]

object SonarSweep:

  def readSonarSweepReport(): SonarSweepReport = Source
    .fromResource("SonarSweepReport.txt")
    .getLines
    .map(_.toInt)

  def numberOfTimesDepthMeasurementsIncrease =
    SonarReportProcessor.determineNoOfDepthIncreases(
      readSonarSweepReport()
    )

  def numberOfTimesTheSumOfDepthMeasurementsInSlidingWindowsOf3Increases =
    SonarReportProcessor.determineNoOfDepthIncreases(
      readSonarSweepReport(),
      windowSize = 3
    )

@main def part1: Unit =
  Result.print(
    question =
      "How many measurements are larger than the previous measurement?",
    answer = SonarSweep.numberOfTimesDepthMeasurementsIncrease
  )

@main def part2: Unit =
  Result.print(
    question = "How many sums are larger than the previous sum?",
    answer =
      SonarSweep.numberOfTimesTheSumOfDepthMeasurementsInSlidingWindowsOf3Increases
  )
