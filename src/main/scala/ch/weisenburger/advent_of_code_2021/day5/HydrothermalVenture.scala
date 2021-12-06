package ch.weisenburger.advent_of_code_2021.day5

import scala.io.Source
import ch.weisenburger.advent_of_code_2021.util.Result


object HydrothermalVenture:
  case class DiagramRange(xAxis: Range, yAxis: Range)

  class LineSegment(x1: Int, y1: Int, x2: Int, y2: Int):
    private val uX = x2 - x1
    private val uY = y2 - y1

    val minX = Math.min(x1, x2)
    val maxX = Math.max(x1, x2)
    val minY = Math.min(y1, y2)
    val maxY = Math.max(y1, y2)

    def isHorizontal = y1 == y2

    def isVertical = x1 == x2

    def isHorizontalOrVertical = isHorizontal || isVertical

    def coversPoint(xP: Int, yP: Int) =
      isWithinRange(xP, yP) && (isHorizontalOrVertical || pointIsOnLineOfSegment(xP, yP))

    private def isWithinRange(xP: Int, yP: Int) =
      xP >= minX && xP <= maxX && yP >= minY && yP <= maxY

    private def pointIsOnLineOfSegment(xP: Int, yP: Int) =
      val rX = (xP - x1).toDouble / uX 
      val rY = (yP - y1).toDouble / uY
      rX == rY

  def readRawlineSegments() = Source
    .fromResource("HydrothermalVents.txt")
    .getLines

  def determineNumberOfPointsWhereAtLeastTwoHorizontalOrVerticalLinesOverlap(
      rawLineSegments: Iterator[String]
  ) =
    val lineSegments = parseRawlineSegments(rawLineSegments).filter(_.isHorizontalOrVertical)
    val diagramRange = determineDiagramRange(lineSegments)
    determineNoOfPointsWithOverlap(diagramRange, lineSegments, overlapThreshold = 2)

  def determineNumberOfPointsWhereAtLeastLinesOverlap(
      rawLineSegments: Iterator[String]
  ) =
    val lineSegments = parseRawlineSegments(rawLineSegments)
    val diagramRange = determineDiagramRange(lineSegments)
    determineNoOfPointsWithOverlap(diagramRange, lineSegments, overlapThreshold = 2)

  private def parseRawlineSegments(rawLineSegments: Iterator[String]) =
    def toInt(str: String) = str.trim.toInt
    val lsRegex = raw"(\d+),(\d+) -> (\d+),(\d+)".r
    rawLineSegments.map { case lsRegex(x1AsStr, y1AsStr, x2AsStr, y2AsStr) =>
      LineSegment(
        x1 = toInt(x1AsStr),
        y1 = toInt(y1AsStr),
        x2 = toInt(x2AsStr),
        y2 = toInt(y2AsStr)
      )
    }.toSeq

  private def determineDiagramRange(lineSegments: Seq[LineSegment]) =
    var xMin = Int.MaxValue
    var xMax = Int.MinValue
    var yMin = Int.MaxValue
    var yMax = Int.MinValue

    lineSegments.foreach { ls =>
      xMin = Math.min(xMin, ls.minX)
      xMax = Math.max(xMax, ls.maxX)
      yMin = Math.min(yMin, ls.minY)
      yMax = Math.max(yMax, ls.maxY)
    }

    DiagramRange(xAxis = (xMin to xMax), yAxis = (yMin to yMax))

  private def determineNoOfPointsWithOverlap(range: DiagramRange, lineSegments: Seq[LineSegment], overlapThreshold: Int) =
      def countCovers(xP: Int, yP: Int) =
          lineSegments.foldLeft(0){
              case (count, lineSegment) if lineSegment.coversPoint(xP,yP) => count + 1
              case (count, _) => count
          }

      val pointsWithOverlapAboveThreshold = for {
          xP <- range.xAxis
          yP <- range.yAxis
          noOfOverlaps = countCovers(xP,yP) if noOfOverlaps >= overlapThreshold
      } yield (xP, yP)
      
      pointsWithOverlapAboveThreshold.size

@main def part1: Unit =
  import HydrothermalVenture._
  Result.print(
    question = "Consider only horizontal and vertical lines. At how many points do at least two lines overlap?",
    answer = determineNumberOfPointsWhereAtLeastTwoHorizontalOrVerticalLinesOverlap(readRawlineSegments())
  )

@main def part2: Unit =
  import HydrothermalVenture._
  Result.print(
    question = "Consider horizontal, vertical and diagonal lines. At how many points do at least two lines overlap?",
    answer = determineNumberOfPointsWhereAtLeastLinesOverlap(readRawlineSegments())
  )