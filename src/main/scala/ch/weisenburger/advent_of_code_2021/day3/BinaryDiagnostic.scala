package ch.weisenburger.advent_of_code_2021.day3

import ch.weisenburger.advent_of_code_2021.util.Result

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

type BinaryNumberStr = String
type DiagnosticReport = Iterator[BinaryNumberStr]

object BinaryDiagnostic:

  def readDiagnosticReport() = Source
    .fromResource("DiagnosticReport.txt")
    .getLines

  def calculatePowerConsumption(gammaRate: Int, epsilonRate: Int) =
    gammaRate * epsilonRate

  def calculateLifeSupportRating(oxygenGeneratorRating: Int, co2ScrubberRating: Int) =
    oxygenGeneratorRating * co2ScrubberRating

  def powerConsumptionBasedOnReport(report: DiagnosticReport) =
    val counters = determineCountersByIndex(report)
    val gammaRateChars = counters.map(_.mostCommonChar)
    val epsilonRateChars = counters.map(_.leastCommonChar)
    calculatePowerConsumption(
      gammaRate = binaryNumToInt(gammaRateChars),
      epsilonRate = binaryNumToInt(epsilonRateChars)
    )

  def powerConsumptionBasedOnDiagnosticReport =
    powerConsumptionBasedOnReport(report = readDiagnosticReport())


  def lifeSupportRatingBasedOnReport(report: DiagnosticReport) =
    val lazyReport = List.from(report)
    val noOfBits = lazyReport.head.length
    val oxygenGeneratorRatingChars = reduceBitByBit(lazyReport, charSelector = (cc) => cc.mostCommonChar, currentBitIdx = 0, noOfBits = noOfBits)
    val co2ScrubberRatingChars = reduceBitByBit(lazyReport, charSelector = (cc) => cc.leastCommonChar, currentBitIdx = 0, noOfBits = noOfBits)
    calculateLifeSupportRating(
      oxygenGeneratorRating = binaryNumToInt(oxygenGeneratorRatingChars), 
      co2ScrubberRating = binaryNumToInt(co2ScrubberRatingChars))
   
  def lifeSupportRatingBasedOnDiagnosticReport =
    lifeSupportRatingBasedOnReport(report = readDiagnosticReport())

  private def reduceBitByBit(report: List[BinaryNumberStr], charSelector: (CharCounter) => Char, currentBitIdx: Int, noOfBits: Int): BinaryNumberStr =
     if(currentBitIdx >= noOfBits) {
       assert(report.length == 1)
       report.head
     } else {
       val counters = determineCountersByIndex(report.iterator)
       val keepWithChar = charSelector(counters(currentBitIdx))
       val filteredReport = report.filter(binNumStr => binNumStr.charAt(currentBitIdx) == keepWithChar)
       val nextBitIdx = currentBitIdx + 1
       reduceBitByBit(filteredReport, charSelector, nextBitIdx, noOfBits)
     }
  

  private def determineCountersByIndex(report: DiagnosticReport) =
    val countersByIndex = collection.mutable.SortedMap.empty[Int, CharCounter]
    for {
      binaryNumberStr <- report
      idx <- (0 until binaryNumberStr.length)
      char = binaryNumberStr.charAt(idx)
      counter = countersByIndex.getOrElseUpdate(idx, CharCounter())
    }(counter.count(char))
    countersByIndex.map((_, counter) => counter).toIndexedSeq

  private def binaryNumToInt(chars: Iterable[Char]) =
    Integer.parseInt(chars.mkString, 2)

  private class CharCounter():
    private val seen =
      collection.mutable.Map.empty[Char, Int]
    def count(c: Char) =
      seen.updateWith(c)(optCount => Some(optCount.getOrElse(0) + 1))
    def mostCommonChar: Char = seen.maxBy(byCountAndCharPrio(prioritizedChar = '1', isMostCommon = true))._1
    def leastCommonChar: Char = seen.minBy(byCountAndCharPrio(prioritizedChar = '0', isMostCommon = false))._1

    /** Determine a "score" for max/min determination
     *
     * Break ties by adding a small amount to the prioritized char,
     * however ensure that non prioritized chars have higher scores if they were more frequent
     */ 
    private def byCountAndCharPrio(prioritizedChar: Char, isMostCommon: Boolean): ((Char, Int)) => Int = {
      case (char, cnt) if char == prioritizedChar =>
        val countBasedScore = cnt * 10
        val prioBasedAdjustment = 1
        if(isMostCommon) countBasedScore + prioBasedAdjustment 
        else countBasedScore - prioBasedAdjustment
      case (_, cnt) => 
        cnt * 10
    }

@main def part1: Unit =
  Result.print(
    question = "What is the power consumption of the submarine?",
    answer = BinaryDiagnostic.powerConsumptionBasedOnDiagnosticReport
  )

@main def part2: Unit =
  Result.print(
    question = "What is the life support rating of the submarine?",
    answer = BinaryDiagnostic.lifeSupportRatingBasedOnDiagnosticReport
  )
