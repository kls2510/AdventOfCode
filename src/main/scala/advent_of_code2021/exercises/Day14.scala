package advent_of_code2021.exercises

import util._


object Day14 extends WithLogger with Exercise[(Map[String, List[String]], Map[String, Long])] {

    val dayNumber: Int = 14

    override def processRawLines(input: List[String]): (Map[String, List[String]], Map[String, Long]) = {
        val mappingsAndString = input.foldLeft((Map(): Map[String, List[String]], "")){
            (agg, lineInfo) => {
                if (lineInfo.contains("->")) {
                    val elements = lineInfo.split(" -> ")
                    val firstChar = elements(0).charAt(0).toString
                    val middleChar = elements(1)
                    val secondChar = elements(0).charAt(1).toString
                    (agg._1 ++ Map(elements(0) -> List(firstChar + middleChar, middleChar + secondChar)), agg._2)
                } else if (lineInfo != "") {
                    (agg._1, lineInfo)
                } else agg
            }
        }
        val stringToPairCount = mappingsAndString._2.sliding(2).map(_.mkString).toList.groupMapReduce(identity)(_ => 1L)(_ + _).toMap
        (mappingsAndString._1, stringToPairCount)
    }


    override def part1(input: (Map[String, List[String]], Map[String, Long])): Unit = {
        logInfo("Starting part 1")
        
        val pairCounts = List.range(0, 10).foldLeft(input._2)(
            (currentPairs, step) => currentPairs.map{case (pair, count) => input._1(pair).map(newPair => (newPair -> count))}.flatten.groupMapReduce(_._1)(_._2)(_ + _)
        )
        val startCounts = pairCounts.toList.map(pairMapping => (pairMapping._1.charAt(0), pairMapping._2)).groupMapReduce(_._1)(_._2)(_ + _)
        val endCounts = pairCounts.toList.map(pairMapping => (pairMapping._1.charAt(1), pairMapping._2)).groupMapReduce(_._1)(_._2)(_ + _)
        val mergedCounts = (startCounts.toList ++ endCounts.toList).groupMapReduce(_._1)(_._2)((c1, c2) => List(c1, c2).max)
        logInfo(mergedCounts.toString)
        logInfo("Result = " + (mergedCounts.values.max - mergedCounts.values.min))
    }

    override def part2(input: (Map[String, List[String]], Map[String, Long])): Unit = {
        logInfo("Starting part 2")
        
        val pairCounts = List.range(0, 40).foldLeft(input._2)(
            (currentPairs, step) => currentPairs.map{case (pair, count) => input._1(pair).map(newPair => (newPair -> count))}.flatten.groupMapReduce(_._1)(_._2)(_ + _)
        )
        val startCounts = pairCounts.toList.map(pairMapping => (pairMapping._1.charAt(0), pairMapping._2)).groupMapReduce(_._1)(_._2)(_ + _)
        val endCounts = pairCounts.toList.map(pairMapping => (pairMapping._1.charAt(1), pairMapping._2)).groupMapReduce(_._1)(_._2)(_ + _)
        val mergedCounts = (startCounts.toList ++ endCounts.toList).groupMapReduce(_._1)(_._2)((c1, c2) => List(c1, c2).max)
        logInfo(mergedCounts.toString)
        logInfo("Result = " + (mergedCounts.values.max - mergedCounts.values.min))
    }
}
