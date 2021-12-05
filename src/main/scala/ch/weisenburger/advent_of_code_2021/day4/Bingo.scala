package ch.weisenburger.advent_of_code_2021.day4

import ch.weisenburger.advent_of_code_2021.util.Result

import scala.io.Source

object Bingo:
  
    case object Marked
    type Number = Int | Marked.type
    class Board(numbers: Array[Array[Number]]):
        private val noOfRows = numbers.length
        private val noOfCols = numbers.head.length

        private var numberOfTries = 0
        private var triesTillBingo: Option[Int] = None 

        def mark(number: Int) = 
            numberOfTries = numberOfTries + 1
            for {
                rowIdx <- (0 until noOfRows)
                colIdx <- (0 until noOfCols)
                boardNumber = numbers(rowIdx)(colIdx) if boardNumber == number
            } (numbers(rowIdx)(colIdx) = Marked)

            if(checkHasBingo() && triesTillBingo.isEmpty) {
                triesTillBingo = Some(numberOfTries)
            }

            this
        
        def hasBingo: Boolean = triesTillBingo.isDefined
        def noOfTriesTillBingo: Option[Int] = triesTillBingo

        def unmarkedNumbers: Seq[Int] = for {
            rowIdx <- (0 until noOfRows)
            colIdx <- (0 until noOfCols)
            boardNumber = numbers(rowIdx)(colIdx) if boardNumber != Marked
        } yield boardNumber.asInstanceOf[Int]
        
        private def checkHasBingo(): Boolean = checkRowsAllNumbersMarked || checkColumnsAllNumbersMarked
        private def checkRowsAllNumbersMarked: Boolean = numbers.exists(row => row.forall(value => value == Marked))
        private def checkColumnsAllNumbersMarked: Boolean = (0 until noOfCols).exists(columnIdx => 
            numbers.forall(row => row(columnIdx) == Marked))


    object Game:
      type Gameplay = Iterable[Board] => Option[Board]
      case class Result(board: Board, lastDrawnNumber: Int)
      def firstBingoWins: Gameplay = boards => boards.find(_.hasBingo)
      def whichBoardHasBingoLast: Gameplay = {
       case boards if boards.forall(_.hasBingo) =>
         boards.maxByOption(_.noOfTriesTillBingo.getOrElse(Int.MinValue))
       case _ => None 
      }

    class Game(randomNumbers: Iterable[Int], boards: Iterable[Board]):  
        def play(gameplay: Game.Gameplay): Game.Result = randomNumbers.iterator.map { drawnNumber =>
            boards.foreach(_.mark(drawnNumber))
            val resBoardOpt = gameplay(boards)
            resBoardOpt.map(winner => Game.Result(winner, drawnNumber))
        }.collectFirst{ case res if res.isDefined => res.get }.get

    def readRawBingoGame() = Source
    .fromResource("BingoGame.txt")
    .getLines.toSeq

    def parseBingoGame(rawGame: Seq[String]): Game =
        val randomNumbers = rawGame.head.trim.split(",").map(_.trim.toInt)
        val boards = rawGame.tail
                        .filterNot(_.isBlank)
                        .grouped(5)
                        .map(rawRows => rawRows.iterator.map(_.trim.split("\\s+").map[Number](_.trim.toInt)).toArray)
                        .map(numbers => Board(numbers))
                        .toSeq
        Game(randomNumbers, boards)
    
    def parseAndPlayRawGame(rawGame: Seq[String], gameplay: Game.Gameplay = Game.firstBingoWins) =
        val game = parseBingoGame(rawGame)
        val winner = game.play(gameplay)
        calculateFinalScore(winner)
    
    private def calculateFinalScore(gameResult: Game.Result) =
        val unmarkedSum = gameResult.board.unmarkedNumbers.sum
        unmarkedSum * gameResult.lastDrawnNumber
    

@main def part1: Unit =
  Result.print(
    question = "What will your final score be if you choose that board?",
    answer = Bingo.parseAndPlayRawGame(Bingo.readRawBingoGame())
  )

@main def part2: Unit =
Result.print(
    question = "Which board will win last, what would its final score be?",
    answer = Bingo.parseAndPlayRawGame(Bingo.readRawBingoGame(), Bingo.Game.whichBoardHasBingoLast)
)
