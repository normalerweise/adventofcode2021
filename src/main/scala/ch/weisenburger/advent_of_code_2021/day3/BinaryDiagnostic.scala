package ch.weisenburger.advent_of_code_2021.day3

import ch.weisenburger.advent_of_code_2021.util.Result

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

type DiagnosticReport = Iterator[String]

object BinaryDiagnostic:

  def readDiagnosticReport() = Source
    .fromResource("DiagnosticReport.txt")
    .getLines

  def calculatePowerConsumption(gammaRate: Int, epsilonRate: Int) =
    gammaRate * epsilonRate

  def powerConsumptionBasedOnReport(report: DiagnosticReport) =
    val counters = determineCountersByIndex(report)
    val gammaRateChars = counters.map(_.mostCommonChar)
    val epsilonRateChars = counters.map(_.leastCommonChar)
    calculatePowerConsumption(
      gammaRate = rateToInt(gammaRateChars),
      epsilonRate = rateToInt(epsilonRateChars)
    )

  def powerConsumptionBasedOnDiagnosticReport =
    powerConsumptionBasedOnReport(report = readDiagnosticReport())

  private def determineCountersByIndex(report: DiagnosticReport) =
    val countersByIndex = collection.mutable.SortedMap.empty[Int, CharCounter]
    for {
      binaryNumberStr <- report
      idx <- (0 until binaryNumberStr.length)
      char = binaryNumberStr.charAt(idx)
      counter = countersByIndex.getOrElseUpdate(idx, CharCounter())
    }(counter.count(char))
    countersByIndex.map((_, counter) => counter)

  private class CharCounter():
    def byCnt: ((Char, Int)) => Int = (_, cnt) => cnt
    private val seen =
      collection.mutable.Map.empty[Char, Int]
    def count(c: Char) =
      seen.updateWith(c)(optCount => Some(optCount.getOrElse(0) + 1))
    def mostCommonChar: Char = seen.maxBy(byCnt)._1
    def leastCommonChar: Char = seen.minBy(byCnt)._1

  private def rateToInt(chars: Iterable[Char]) =
    Integer.parseInt(chars.mkString, 2)

@main def part1: Unit =
  Result.print(
    question = "What is the power consumption of the submarine?",
    answer = BinaryDiagnostic.powerConsumptionBasedOnDiagnosticReport
  )

@main def part2: Unit =
  Result.print(
    question = "",
    answer = 42
  )
