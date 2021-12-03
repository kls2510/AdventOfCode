package advent_of_code2021.exercises

import scala.io.Source

import util._


object Day3 extends WithLogger with Exercise[List[Short]] {

  override val dayNumber: Int = 3
  val bitLen: Short = 12
  val shortLen: Short = 16
  val base = Integer.parseInt(List.range(0: Short, 16).map(i => if (i < shortLen - bitLen) "0" else "1").mkString(""), 2).toShort

  override def processRawLines(input: List[String]): List[Short] = {
    input.map(line => Integer.parseInt(line, 2).toShort)
  }

  def findMostCommonBit(bits: List[Short]): Short = {
      val bitCounts = bits.groupMapReduce(identity)(_ => 1)(_+_)
      if (bitCounts.get(0) == bitCounts.get(1)) 1 else bitCounts.maxBy(_._2)._1
  }

  def findMostCommonBits(input: List[Short]): Short = {
    List.range(0: Short, bitLen).foldLeft(0: Short){
        (acc, index) => {
            val selector = (Integer.parseInt("1", 2) << index).toShort
            val allBits = input.map(e => ((e & selector) >> index).toShort)
            val mostCommonBit = findMostCommonBit(allBits)
            (acc | (mostCommonBit << index)).toShort
        }
    }
  }

  def findOppositeNumber(number: Short): Short = {
      (number ^ base).toShort
  }

  override def part1(input: List[Short]): Unit = {
    val reducedNumber = findMostCommonBits(input)
    val opposite = findOppositeNumber(reducedNumber)
    val answer = reducedNumber * opposite
    logInfo("Found " + reducedNumber.toBinaryString + " and " + opposite.toBinaryString)
    logInfo("Answer = " + answer.toString)
  }

  def filterList(input: List[Short]): (Short, Short) = {
    val (l1, l2) = List.range(1: Short, bitLen + 1).foldLeft((List(): List[Short], List(): List[Short])){
        (acc, index) => {
            val shift = bitLen - index
            val selector = (Integer.parseInt("1", 2) << shift).toShort
            val maskAndShift = (x: Short) => ((x & selector) >> shift).toShort

            val targetMax = findMostCommonBit(acc._1.map(maskAndShift(_)))
            val targetMin = (findMostCommonBit(acc._2.map(maskAndShift(_))) ^ Integer.parseInt("1", 2)).toShort
            val partitionFun = (x: Short, target: Short) => maskAndShift(x) == target

            acc match {
                case (Nil, Nil) => {
                    val startTarget = findMostCommonBit(input.map(maskAndShift(_)))
                    input.partition(partitionFun(_, startTarget))
                }
                case (n1:: Nil, n2 :: Nil) => (List(n1), List(n2))
                case (l1, n2 :: Nil) => (l1.filter(partitionFun(_, targetMax)), List(n2))
                case (n1 :: Nil, l2) => (List(n1), l2.filter(partitionFun(_, targetMin)))
                case (l1, l2) => (l1.filter(partitionFun(_, targetMax)), l2.filter(partitionFun(_, targetMin)))
            }
        }
    }
    logInfo("Final l1 = " + l1.toString())
    logInfo("Final l2 = " + l2.toString())
    (l1.head, l2.head)
  }

  override def part2(input: List[Short]): Unit = {
    val (n1, n2) = filterList(input)
    logInfo("Result " + (n1*n2).toString())
  }

}
