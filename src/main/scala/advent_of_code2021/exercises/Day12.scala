package advent_of_code2021.exercises

import util._


case class Graph(bidirectionalEdges: List[String]) extends WithLogger {
    val adjacency = bidirectionalEdges.map(s => {
        val nodes = s.split("-")
        List((nodes(0), nodes(1)), (nodes(1), nodes(0)))
    }).flatten.groupMapReduce(_._1)(n => List(n._2))(_ ++ _)
    val nodes = adjacency.keySet

    def countPathsBetweenStartAndEnd: Int = countPaths("start", "end", Set())

    private def countPaths(start: String, end: String, visited: Set[String]): Int = {
        if (start == end) 1 else {
            val neighbours = adjacency(start).filter(!visited.contains(_))
            val newVisiteds = if (start.toCharArray.map(_.isUpper).forall(identity)) visited else visited + start
            neighbours.foldLeft(0)((pathCount, neighbour) => pathCount + countPaths(neighbour, end, newVisiteds))
        }
    }

    private def pathsWithRepeats(start: String, end: String, repeat: String, repeatNumVisits: Int, visited: Set[String], path: List[String]): Set[String] = {
        if (start == end) Set((end :: path).reverse.mkString(" ")) else {
            val neighbours = adjacency(start).filter(!visited.contains(_))
            val newVisiteds = if (start.toCharArray.map(_.isUpper).forall(identity) || start == repeat) visited else visited + start
            val newRepeatNumVisits = if (start == repeat) repeatNumVisits + 1 else repeatNumVisits
            val newVisitedsIncRepeat = if (start == repeat && newRepeatNumVisits == 2) newVisiteds + repeat else newVisiteds
            neighbours.foldLeft(Set(): Set[String])((paths, neighbour) => {
                val neighbourPaths = pathsWithRepeats(neighbour, end, repeat, newRepeatNumVisits, newVisitedsIncRepeat, start :: path)
                neighbourPaths | paths
            })
        }
    }

    def countPathsBetweenStartAndEndWithRepeats: Int = {
        val smallCaves = nodes.filter(!List("start", "end").contains(_)).filter(!_.toCharArray.map(_.isUpper).forall(identity))
        smallCaves.foldLeft(Set(): Set[String])(
            (allPaths, repeatCave) => allPaths ++ pathsWithRepeats("start", "end", repeatCave, 0, Set(), List())
        ).size
    }
}


object Day12 extends WithLogger with Exercise[Graph] {

    val dayNumber: Int = 12

    override def processRawLines(input: List[String]): Graph = {
        Graph(input)
    }


    override def part1(input: Graph): Unit = {
        logInfo("Starting part 1")
        logInfo("num paths = " + input.countPathsBetweenStartAndEnd.toString)
    }

    override def part2(input: Graph): Unit = {
        logInfo("Starting part 2")
        logInfo("num paths = " + input.countPathsBetweenStartAndEndWithRepeats.toString)        
    }
}
