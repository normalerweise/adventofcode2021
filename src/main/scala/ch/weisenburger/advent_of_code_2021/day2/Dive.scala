package ch.weisenburger.advent_of_code_2021.day2

import scala.io.Source
import javax.naming.directory.DirContext
import ch.weisenburger.advent_of_code_2021.day2.Navigator.Position
import ch.weisenburger.advent_of_code_2021.util.Result

object Direction:
  def fromStrValue(str: String) = str match {
    case "forward" => Forward
    case "down"    => Down
    case "up"      => Up
  }

enum Direction:
  case Forward, Down, Up

case class Command(direction: Direction, amount: Int)

type Course = Iterator[Command]

object Dive:

  def readPlannedCourse(): Course =
    val commandRegex = raw"(forward|down|up)\s(\d)".r
    def parseCourseLine(line: String) = line match {
      case commandRegex(directionAsStr, amountAsStr) =>
        Command(
          direction = Direction.fromStrValue(directionAsStr),
          amount = amountAsStr.toInt
        )
    }

    Source
      .fromResource("PlannedCourse.txt")
      .getLines
      .map(parseCourseLine)

  def followCourse(
      course: Course,
      instructionSet: Navigator.InstructionSet
  ) =
    Navigator.followCourse(course, instructionSet)

  def finalPositionResult(finalPosition: Position) =
    finalPosition.horizontal * finalPosition.depth

  def multipleOfFinalPositionAfterFollowingCourse(
      course: Course,
      instructionSet: Navigator.InstructionSet = Navigator.part1Instructions
  ) =
    finalPositionResult(followCourse(course, instructionSet))

  def multipleOfFinalPositionAfterFollowingCourseUsingeNewInstructions(
      course: Course
  ) =
    finalPositionResult(
      followCourse(course, instructionSet = Navigator.part2Instructions)
    )

  def multipleOfFinalPositionAfterFollowingPlannedCourse =
    multipleOfFinalPositionAfterFollowingCourse(course = readPlannedCourse())

  def multipleOfFinalPositionAfterFollowingPlannedCourseUsingeNewInstructions =
    multipleOfFinalPositionAfterFollowingCourseUsingeNewInstructions(
      course = readPlannedCourse()
    )

@main def part1: Unit =
  Result.print(
    question =
      "What do you get if you multiply your final horizontal position by your final depth?",
    answer = Dive.multipleOfFinalPositionAfterFollowingPlannedCourse
  )

@main def part2: Unit =
  Result.print(
    question = "Using this new interpretation of the commands: " +
      "What do you get if you multiply your final horizontal position by your final depth?",
    answer =
      Dive.multipleOfFinalPositionAfterFollowingPlannedCourseUsingeNewInstructions
  )
