package advent_of_code2021.exercises

import util._


case class BingoBoard(board: Map[Int, (Int, Int)], boardSize: Int = 5) extends WithLogger {
    val coordToNumber = board.map(_.swap)
    val drawGrid: List[List[Int]] = List.range(0, boardSize).map(
        rowNo => {
            List.range(0, boardSize).map(
                colNo => coordToNumber((rowNo, colNo))
            )
        }
    )

    def checkColumnsFn(grid: List[List[Int]]): Boolean = grid.transpose.filter(arr => arr.forall(i => i == -1)).isEmpty
    def checkRowsFn(grid: List[List[Int]]): Boolean = grid.filter(arr => arr.forall(i => i == -1)).isEmpty

    def markNumbersFn(numbers: List[Int]): (List[Int], List[List[Int]]) = {
        val boardNumbers = drawGrid
        val res = numbers.foldLeft((List(): List[Int], boardNumbers)){
            (acc, number) => {
                val coordOpt = board.get(number)
                if (checkColumnsFn(acc._2) && checkRowsFn(acc._2)) {
                    coordOpt match {
                        case Some(coord) => (number :: acc._1, acc._2.updated(coord._1, acc._2(coord._1).updated(coord._2, -1)))
                        case None => (number :: acc._1, acc._2)
                    }
                } else acc
            }
        }
        (res._1.reverse, res._2)
    }

}

object Day4 extends WithLogger with Exercise[(List[Int], List[BingoBoard])] {

    val dayNumber: Int = 4
    val boardSize: Int = 5

    override def processRawLines(input: List[String]): (List[Int], List[BingoBoard]) = {
        val reading = input.zipWithIndex.foldLeft((List(): List[Int], Map(): Map[Int, (Int, Int)], List(): List[BingoBoard])){
            (agg, lineInfo) => {
                lineInfo match {
                    case (l, 0) => (l.trim.split(",").toList.map(_.toInt), agg._2, agg._3)
                    case ("", _) => {
                        if (!agg._2.isEmpty) {
                            (agg._1, Map(), BingoBoard(agg._2) :: agg._3)
                        } else agg
                    }
                    case (l, _) => {
                        val currentNumRows = agg._2.size / boardSize
                        val newMap = lineInfo._1.trim.split(" +").zipWithIndex.foldLeft(agg._2){
                            (m, entry) => {
                                val coord = (currentNumRows, entry._2)
                                m + (entry._1.trim.toInt -> coord)
                            }
                        }
                        (agg._1, newMap, agg._3)
                    }
                }
            }
        }
        (reading._1, reading._3.reverse)
    }

    override def part1(input: (List[Int], List[BingoBoard])): Unit = {
        logInfo("Starting part 1")
        val results = input._2.map(_.markNumbersFn(input._1))
        val allInputs = results.map(_._1)
        val allGrids = results.map(_._2)

        val lengths = allInputs.map(_.length)
        val leastNumberIndex = lengths.indexOf(lengths.min)
        val leastCalledNumbers = allInputs(leastNumberIndex)

        logInfo("Bingo board index = " + leastNumberIndex.toString)
        logInfo("Bingo board = " + input._2(leastNumberIndex).drawGrid.toString)
        logInfo("Least numbers = " + lengths(leastNumberIndex).toString)
        logInfo("Calls = " + leastCalledNumbers.toString)

        val n1 = allGrids(leastNumberIndex).flatten.filter(_ >= 0).reduce(_ + _)
        val n2 = leastCalledNumbers.takeRight(1).head
        logInfo("Result = " + n1.toString + " * " + n2.toString + " = " + (n1*n2).toString)
    }

    override def part2(input: (List[Int], List[BingoBoard])): Unit = {
        val results = input._2.map(_.markNumbersFn(input._1))
        val allInputs = results.map(_._1)
        val allGrids = results.map(_._2)

        val lengths = allInputs.map(_.length)
        val mostNumberIndex = lengths.indexOf(lengths.max)
        val mostCalledNumbers = allInputs(mostNumberIndex)

        logInfo("Bingo board index = " + mostNumberIndex.toString)
        logInfo("Bingo board = " + input._2(mostNumberIndex).drawGrid.toString)
        logInfo("Most numbers = " + lengths(mostNumberIndex).toString)
        logInfo("Calls = " + mostCalledNumbers.toString)

        val n1 = allGrids(mostNumberIndex).flatten.filter(_ >= 0).reduce(_ + _)
        val n2 = mostCalledNumbers.takeRight(1).head
        logInfo("Result = " + n1.toString + " * " + n2.toString + " = " + (n1*n2).toString)
    }
}
