package advent_of_code2021.exercises

import util._


object Day8 extends WithLogger with Exercise[List[(List[List[Char]], List[List[Char]])]] {

    val dayNumber: Int = 8
    val numSegmentsToNumbers = Map(
        2 -> List(1),
        3 -> List(7),
        4 -> List(4),
        5 -> List(2, 3, 5),
        6 -> List(0, 6, 9),
        7 -> List(8),
    )
    val numberToCorrectSegments= Map(
        0 -> List('a', 'b', 'c', 'e', 'f', 'g'),
        1 -> List('c', 'f'),
        2 -> List('a', 'c', 'd', 'e', 'g'),
        3 -> List('a', 'c', 'd', 'f', 'g'),
        4 -> List('b', 'c', 'd', 'f'),
        5 -> List('a', 'b', 'd', 'f', 'g'),
        6 -> List('a', 'b', 'd', 'e', 'f', 'g'),
        7 -> List('a', 'c', 'f'),
        8 -> List('a', 'b', 'c', 'd', 'e', 'f', 'g'),
        9 -> List('a', 'b', 'c', 'd', 'f', 'g'),
    )
    val allSegments = numberToCorrectSegments.values.map(_.mkString).toList
    val charsToNumber = numberToCorrectSegments.map{case (k, v) => v.mkString -> k}

    override def processRawLines(input: List[String]): List[(List[List[Char]], List[List[Char]])] = {
        val outputReversed = input.foldLeft((List(): List[List[List[Char]]], List(): List[List[List[Char]]]))(
            (output, line) => {
                val signalsAndOutputs = line.split(" \\| ").toList
                val newSignals = signalsAndOutputs.head.split(" ").map(_.toCharArray.toList).toList
                val newTargets = signalsAndOutputs.reverse.head.split(" ").map(_.toCharArray.toList).toList
                (newSignals :: output._1, newTargets :: output._2)
            }
        )
        outputReversed._1.reverse zip outputReversed._2.reverse
    }

    override def part1(input: List[(List[List[Char]], List[List[Char]])]): Unit = {
        logInfo("Starting part 1")
        
        val output = input.map(_._2)
        val guaranteedNumbers = output.map(outputCharArrs => outputCharArrs.filter(entry => numSegmentsToNumbers(entry.length).length == 1))
        logInfo("Num easy numbers = " + guaranteedNumbers.flatten.length)
    }

    override def part2(input: List[(List[List[Char]], List[List[Char]])]): Unit = {
        logInfo("Starting part 2")

        val allNumbers = input.map{
            case (signals, output) => {
                val guaranteedSignals = signals.filter(entry => numSegmentsToNumbers(entry.length).length == 1)
                val guaranteedToTarget = guaranteedSignals.map(guaranteedSignal => {
                    val number = numSegmentsToNumbers(guaranteedSignal.length).head
                    (guaranteedSignal, number, numberToCorrectSegments(number))
                })
                val mappingsToCheck = guaranteedToTarget.map{
                    case (mixedSignal, number, targetSignal) => {
                        val allTargetPerms = targetSignal.permutations
                        allTargetPerms.map(perm => mixedSignal zip perm).toList.map(combo => combo.map(t => t._1 -> t._2).toMap)
                    }
                }
                val mergedMappingsWithNoContradications = mappingsToCheck.tail.foldLeft(mappingsToCheck.head)(
                    (validMappings, nextPotentials) => {
                        nextPotentials.map(
                            newMapping => {
                                val allowedToKeepMappings = validMappings.filter(currentMap => {
                                    val nonContradictingValues = newMapping.keySet.takeWhile(key => {
                                        currentMap.get(key) match {
                                            case Some(v) => newMapping(key) == v
                                            case None => true
                                        }
                                    })
                                    nonContradictingValues.size == newMapping.size
                                })
                                allowedToKeepMappings.map(oldMapping => oldMapping ++ newMapping)
                            }
                        ).flatten
                    }
                )
                val finalFailedTranslators = mergedMappingsWithNoContradications.takeWhile(
                    translator => {
                        output.takeWhile(
                            scrambled => allSegments.contains(scrambled.map(translator(_)).sorted.mkString)
                        ).length < 4
                    })
                val validTranslator = mergedMappingsWithNoContradications(finalFailedTranslators.length)
                val finalOutputs = output.map(
                    scrambled => scrambled.map(validTranslator(_)).sorted
                )

                val numberList = finalOutputs.map(chars => charsToNumber(chars.mkString))
                numberList.mkString.toInt
            }
        }
        logInfo("Numbers: " + allNumbers.length + " " + allNumbers.toString)
        logInfo("Sum = " + allNumbers.reduce(_ + _))
    }
}
