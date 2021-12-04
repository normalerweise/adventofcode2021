package ch.weisenburger.advent_of_code_2021.day2

object Navigator:

  type InstructionSet =
    (currentPosition: Position, command: Command) => Position

  private object Position:
    def initial = Position(horizontal = 0, depth = 0, aim = 0)

  case class Position(horizontal: Int, depth: Int, aim: Int)

  val part1Instructions =
    (currentPosition: Position, command: Command) =>
      import Direction.*
      command.direction match {
        case Forward =>
          currentPosition.copy(horizontal =
            currentPosition.horizontal + command.amount
          )
        case Down =>
          currentPosition.copy(depth = currentPosition.depth + command.amount)
        case Up =>
          currentPosition.copy(depth = currentPosition.depth - command.amount)
      }

  val part2Instructions =
    (currentPosition: Position, command: Command) =>
      import Direction.*
      command.direction match {
        case Forward =>
          currentPosition.copy(
            horizontal = currentPosition.horizontal + command.amount,
            depth =
              currentPosition.depth + (currentPosition.aim * command.amount)
          )
        case Down =>
          currentPosition.copy(aim = currentPosition.aim + command.amount)
        case Up =>
          currentPosition.copy(aim = currentPosition.aim - command.amount)
      }

  def followCourse(
      course: Course,
      instructionSet: InstructionSet
  ): Position =
    course.foldLeft(Position.initial)(instructionSet)
