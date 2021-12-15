package advent_of_code2021.exercises

import collection.mutable.PriorityQueue

import util._


object Day15 extends WithLogger with Exercise[Vector[Vector[Int]]] {

    val dayNumber: Int = 15

    override def processRawLines(input: List[String]): Vector[Vector[Int]] = {
        input.foldLeft(Vector(): Vector[Vector[Int]])(
            (allLines, line) => allLines :+ line.toCharArray.toVector.map(c => Integer.parseInt(c.toString))
        )
    }

    def calcH(coord: (Int, Int), end: (Int, Int)): Int = (end._1 - coord._1) + (end._2 - coord._2)

    def getEntry(coord: (Int, Int), end: (Int, Int), currentCost: Int): ((Int, Int), (Int, Int, Boolean)) = (coord, (currentCost, currentCost + calcH(coord, end), false))

    def getNeighbours(coords: (Int, Int), width: Int, height: Int): List[(Int, Int)] = {
        val row = coords._1
        val col = coords._2
        val up = if (row > 0) Some((row - 1, col)) else None
        val down = if (row + 1 < height) Some((row + 1, col)) else None
        val left = if (col > 0) Some((row, col - 1)) else None
        val right = if (col + 1 < width) Some((row, col + 1)) else None
        List(up, down, left, right).filter(_.isDefined).map(_.get)
    }

    def Astar(activeSet: PriorityQueue[((Int, Int), Int)], scores: Map[(Int, Int), (Int, Int, Boolean)], end: (Int, Int), grid: Vector[Vector[Int]]): Int = {
        var pq = activeSet
        def AstarRec(_scores: Map[(Int, Int), (Int, Int, Boolean)]): Int = {
            if (pq.isEmpty) Int.MaxValue else {
                val bestNextStep = pq.dequeue
                val coord = bestNextStep._1
                val currentRealScore = _scores(bestNextStep._1)._1
                val currentPredictedScore = _scores(bestNextStep._1)._2

                if (coord == end) currentRealScore
                else {
                    val neighbours = getNeighbours(coord, end._1 + 1, end._2 + 1)
                    val newScores = neighbours.foldLeft(_scores + (coord -> (currentRealScore, currentPredictedScore, true)))(
                        (nextScores, neighbour) => {
                            val neighbourRealScore = currentRealScore + grid(neighbour._1)(neighbour._2)
                            val potentialNextEntry = getEntry(neighbour, end, neighbourRealScore)
                            nextScores.get(neighbour) match {
                                case Some((_, _, true)) => nextScores
                                case Some((previousRealScore, _, false)) => if (neighbourRealScore < previousRealScore) {
                                    pq.mapInPlace(e => if (e._1 == neighbour) (e._1, potentialNextEntry._2._2) else e)
                                    pq = pq.clone
                                    nextScores + (potentialNextEntry._1 -> potentialNextEntry._2)
                                } else nextScores
                                case None => {
                                    pq.enqueue((potentialNextEntry._1, potentialNextEntry._2._2))
                                    nextScores + (potentialNextEntry._1 -> potentialNextEntry._2)
                                }
                            }
                        }
                    )
                    AstarRec(newScores)
                }
            }
        }
        AstarRec(scores)
    }

    override def part1(input: Vector[Vector[Int]]): Unit = {
        logInfo("Starting part 1")
        val start = (0, 0)
        val end = (input.length - 1, input(0).length - 1)
        val firstNode = getEntry(start, end, 0)
        val lowestRisk = Astar(PriorityQueue((firstNode._1, firstNode._2._2)){ case (e1, e2) => if (e1._2 > e2._2) -1 else 1 }, Map(firstNode._1 -> firstNode._2), end, input)
        logInfo("Lowest risk = " + lowestRisk.toString)
    }

    override def part2(input: Vector[Vector[Int]]): Unit = {
        logInfo("Starting part 2")
        
        val input2 = Vector(Vector(8))
        val allCols = for (
            across <- 0 to 4
        ) yield {
            val colSquares = for (
                down <- 0 to 4
            ) yield {
                input.map(row => row.map(cell => {
                    val newTotal = cell + across + down
                    if (newTotal % 9 == 0) 9 else newTotal % 9
                }))
            }
            colSquares.reduce(_ ++ _)
        }
        val finalGrid = allCols.tail.foldLeft(allCols.head: Vector[Vector[Int]]) (
            (acc, col) => (acc zip col).map{ case (c1, c2) => c1 ++ c2 }
        )
        logInfo(finalGrid.size.toString + " " + finalGrid(0).size.toString)

        val start = (0, 0)
        val end = (finalGrid.length - 1, finalGrid(0).length - 1)
        val firstNode = getEntry(start, end, 0)
        val lowestRisk = Astar(PriorityQueue((firstNode._1, firstNode._2._2)){ case (e1, e2) => if (e1._2 > e2._2) -1 else 1 }, Map(firstNode._1 -> firstNode._2), end, finalGrid)
        logInfo("Lowest risk = " + lowestRisk.toString)
    }
}
