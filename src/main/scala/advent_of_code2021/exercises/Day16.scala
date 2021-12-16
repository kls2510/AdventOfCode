package advent_of_code2021.exercises

import collection.mutable.PriorityQueue

import util._
import java.math.BigInteger


trait Packet {
    val version: Int;
    val typ: Int;
    val numBits: Int;
    val versionSum: Int;
    val value: Long;
}

case class LiteralPacket(version: Int, typ: Int, contents: String) extends Packet {
    val value = new BigInteger(contents.sliding(5, 5).map(s => s.tail).mkString, 2).longValue() 

    val numBits = 3 + 3 + contents.length

    val versionSum = version
}

case class OperatorPacket(version: Int, typ: Int, lengthTypeId: Int, length: Int, contents: List[Packet]) extends Packet {
    val value = typ match {
        case 0 => contents.tail.foldLeft(contents.head.value)((acc, packet) => acc + packet.value)
        case 1 => contents.tail.foldLeft(contents.head.value)((acc, packet) => acc * packet.value)
        case 2 => contents.tail.foldLeft(contents.head.value)((acc, packet) => List(acc, packet.value).min)
        case 3 => contents.tail.foldLeft(contents.head.value)((acc, packet) => List(acc, packet.value).max)
        case 5 => contents match {
            case p1 :: p2 :: Nil => if (p1.value > p2.value) 1 else 0
        }
        case 6 => contents match {
            case p1 :: p2 :: Nil => if (p1.value < p2.value) 1 else 0
        }
        case 7 => contents match {
            case p1 :: p2 :: Nil => if (p1.value == p2.value) 1 else 0
        }
    }

    val numBits = 3 + 3 + 1 + (if (lengthTypeId == 0) 15 else 11) + contents.map(p => p.numBits).reduce(_ + _)

    val versionSum = version + contents.map(_.versionSum).reduce(_ + _)
}

object Day16 extends WithLogger with Exercise[List[Char]] {

    val dayNumber: Int = 16

    override def processRawLines(input: List[String]): List[Char] = {
        input(0).toCharArray().toList
    }

    def Hex2Bin(input: List[Char]): String = {
        input.reverse.zipWithIndex.foldLeft(""){
            case (total, (hex, index)) => {
                val bits = Integer.parseInt(hex.toString, 16)
                val bitStr = bits.toBinaryString
                val padding = List.range(0, 4 - bitStr.length).map(_ => 0).mkString
                padding + bitStr + total
            }
        }
    }

    // version, packet type, rest
    def popHeader(input: String): (Int, Int, String) = {
        val version = Integer.parseInt(input.take(3), 2)
        val rem1 = input.drop(3)
        val typ = Integer.parseInt(rem1.take(3), 2)
        val rem2 = rem1.drop(3)
        (version, typ, rem2)
    }

    def popOperatorHeader(input: String): (Int, Int, String) = {
        val lengthType = Integer.parseInt(input.head.toString, 2)
        val rem1 = input.drop(1)
        val numBits = if (lengthType == 0) 15 else 11
        (lengthType, Integer.parseInt(rem1.take(numBits), 2), rem1.drop(numBits))
    }

    def parseToPacket(input: String): Packet = {
        val (version, packetType, rest) = popHeader(input)
        if (packetType == 4) {
            // literal
            val contentsStart = rest.sliding(5, 5).takeWhile(_.head == '1').mkString
            val next1 = rest.drop(contentsStart.length)
            val contents = contentsStart + next1.take(5)
            LiteralPacket(version, packetType, contents)
        } else {
            // op
            val (lengthTypeId, length, remainder) = popOperatorHeader(rest)
            val subPackets = if (lengthTypeId == 0) {
                // number of bits
                val subPacketsReversed = List.range(0, length).foldLeft(0, List(): List[Packet], remainder){
                    case ((nextStart, packets, nextInput), index) => {
                        if (nextStart < length) {
                            val newPacket = parseToPacket(nextInput)
                            val newPacketLength = newPacket.numBits
                            (nextStart + newPacketLength, newPacket :: packets, nextInput.drop(newPacketLength))
                        } else (nextStart, packets, nextInput)
                    }
                }
                subPacketsReversed._2.reverse
            } else {
                // number of sub packets
                val subPacketsReversed = List.range(0, length).foldLeft(List(): List[Packet], remainder){
                    case ((packets, nextInput), index) => {
                        val newPacket = parseToPacket(nextInput)
                        val newPacketLength = newPacket.numBits
                        (newPacket :: packets, nextInput.drop(newPacketLength))
                    }
                }
                subPacketsReversed._1.reverse
            }
            OperatorPacket(version, packetType, lengthTypeId, length, subPackets)
        }
    }

    override def part1(input: List[Char]): Unit = {
        logInfo("Starting part 1")
        val parsedHex = Hex2Bin(input)
        val packet = parseToPacket(parsedHex)
        logInfo("Sum of versions = " + packet.versionSum.toString)
    }

    override def part2(input: List[Char]): Unit = {
        logInfo("Starting part 2")
        val parsedHex = Hex2Bin(input)
        val packet = parseToPacket(parsedHex)
        logInfo("Calculation output = " + packet.value.toString)
    }
}
