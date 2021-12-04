package ch.weisenburger.advent_of_code_2021.day1

object SonarReportProcessor:

  private object State:
    def initial =
      State(noOfIncreases = 0, previousSum = None)

  private case class State(
      noOfIncreases: Int,
      previousSum: Option[Int]
  )

  def determineNoOfDepthIncreases(
      sonarSweepReport: SonarSweepReport,
      windowSize: Int = 1
  ): Int =
    sonarSweepReport
      .sliding(windowSize)
      .withPartial(false)
      .foldLeft(State.initial)(process)
      .noOfIncreases

  private def process(state: State, window: Seq[Int]) =
    val currentSum = window.sum
    state.previousSum match {
      case Some(previousSum) if currentSum > previousSum =>
        state.copy(
          noOfIncreases = state.noOfIncreases + 1,
          previousSum = Some(currentSum)
        )
      case _ =>
        state.copy(previousSum = Some(currentSum))
    }
