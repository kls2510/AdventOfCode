package advent_of_code2021.exercises

import util._

object Scanner {
    def matchPairs(pairs1: List[(Vec3, Vec3)], pairs2: List[(Vec3, Vec3)]): (List[Vec3], List[Vec3]) = {
        // 1 => current pairs, 2 => new pairs to match
        def _matchPairs(current: List[(Vec3, Vec3)], toAdd: List[(Vec3, Vec3)], currentDiff: Option[Vec3], currentMapping: Map[(Vec3, Vec3), (Vec3, Vec3)]): Option[Map[(Vec3, Vec3), (Vec3, Vec3)]] = {
            toAdd match {
                case nextPair :: _ => {
                    currentDiff match {
                        case Some(value) => {
                            val findMatchingPairs = current.filter(currentPair => {
                                val d = currentPair._2 - currentPair._1
                                ((d == nextPair._2 - nextPair._1) && (currentPair._1 - nextPair._1 == value)) || ((d == nextPair._1 - nextPair._2) && (currentPair._2 - nextPair._1 == value))
                            })
                            val maybeRes = findMatchingPairs.foldLeft(None: Option[Map[(Vec3, Vec3), (Vec3, Vec3)]])(
                                (mapping, pairingAttempt) => {
                                    mapping match {
                                        case Some(m) => Some(m)
                                        case _ => _matchPairs(current.filterNot(_ == pairingAttempt), toAdd.tail, currentDiff, currentMapping + (pairingAttempt -> nextPair))
                                    }
                                }
                            )
                            maybeRes.orElse(_matchPairs(current, toAdd.tail, currentDiff, currentMapping))
                        }
                        case None => {
                            val findMatchingPairs = current.filter(currentPair => {
                                val d = currentPair._2 - currentPair._1
                                (d == nextPair._2 - nextPair._1) || (d == nextPair._1 - nextPair._2)
                            })
                            val maybeRes = findMatchingPairs.foldLeft(None: Option[Map[(Vec3, Vec3), (Vec3, Vec3)]])(
                                (mapping, pairingAttempt) => {
                                    mapping match {
                                        case Some(m) => Some(m)
                                        case _ => {
                                            val newTarget = pairingAttempt._1 - nextPair._1
                                            _matchPairs(current.filterNot(_ == pairingAttempt), toAdd.tail, Some(newTarget), currentMapping + (pairingAttempt -> nextPair))
                                        }
                                    }
                                }
                            )
                            maybeRes.orElse(_matchPairs(current, toAdd.tail, None, currentMapping))
                        }
                    }
                }
                case _ => {
                    if (currentMapping.map(entry => List(entry._1._1, entry._2._2)).flatten.toList.distinct.length >= 12) Some(currentMapping) else None
                }
            }
        }
        val res = _matchPairs(pairs1, pairs2, None, Map())
        res.map(_.toList.foldLeft((List(), List()): (List[Vec3], List[Vec3])){
            case ((l1, l2), (original, added)) => {
                if (original._1 - original._2 == added._1 - added._2) {
                    ((original._1 :: original._2 :: l1).distinct, (added._1 :: added._2 :: l2).distinct)
                } else ((original._2 :: original._1 :: l1).distinct, (added._1 :: added._2 :: l2).distinct)
            }
        }).getOrElse((List(), List()))
    }
}

case class Scanner(beacons: List[Vec3]) extends WithLogger {
    val allOrientations = beacons.map(
        original => {
            List.range(0, 4).map(numXRot => List.range(0, 4).map(numYRot => List.range(0, 4).map(numZRot => {
                val rotx = List.range(0, numXRot).foldLeft(original){case (v, _) => v.rotateX}
                val roty = List.range(0, numYRot).foldLeft(rotx){case (v, _) => v.rotateY}
                List.range(0, numZRot).foldLeft(roty){case (v, _) => v.rotateZ}
            })).flatten).flatten.distinct
        }
    )

    def getAllBeaconOrientations(beaconNumber: Int): List[Vec3] = allOrientations(beaconNumber)

    def getBeaconAtOrientation(beaconNumber: Int, orientation: Int): Vec3 = allOrientations(beaconNumber)(orientation)

    def getAllBeaconsAtOrientation(orientation: Int): List[Vec3] = allOrientations.map(_(orientation))

    def getAllPairsAtOrientation(orientation: Int): List[(Vec3, Vec3)] = {
        val sensorsAtOrientation = getAllBeaconsAtOrientation(orientation)
        (for (
            i <- 0 to sensorsAtOrientation.length - 1;
            j <- i+1 to sensorsAtOrientation.length - 1
            ) yield (sensorsAtOrientation(i), sensorsAtOrientation(j))
        ).toList
    }

    def overlap(other: Scanner, baseOrientation: Int): Option[(Scanner, Vec3)] = {
        val allCurrentPairs = getAllPairsAtOrientation(baseOrientation)
        for (
            i <- 0 to 23
        ) {
            val allNewPairs = other.getAllPairsAtOrientation(i)
            val maybeValidMapping = Scanner.matchPairs(allCurrentPairs, allNewPairs)
            if (maybeValidMapping._1.length > 0) {
                logInfo("Overlap found scanner 0 in orientation " + baseOrientation + " and other scanner in orientation " + i)
                val offSet = (maybeValidMapping._1 zip maybeValidMapping._2).map{ case (v1, v2) => v1 - v2 }.distinct.head
                logInfo("Offset = " + offSet)
                val newBeacons = other.getAllBeaconsAtOrientation(i).map(oldBasis => oldBasis + offSet)
                return Some((Scanner((newBeacons ++ beacons).distinct), offSet))
            }
        }
        None
    }
}

object Day19 extends WithLogger with Exercise[List[Scanner]] {

    val dayNumber: Int = 19

    override def processRawLines(input: List[String]): List[Scanner] = {
        val scanners = input.foldLeft((List(): List[Vec3], List(): List[Scanner])){
            (agg, lineInfo) => {
                if (lineInfo == "") {
                    (List(), Scanner(agg._1.reverse) :: agg._2)
                }
                else if (lineInfo.startsWith("---")) agg
                else {
                    val coords = lineInfo.split(",").map(Integer.parseInt(_))
                    val v = Vec3(coords(0), coords(1), coords(2))
                    (v :: agg._1, agg._2)
                }
            }
        }
        (Scanner(scanners._1.reverse) :: scanners._2).reverse
    }

    def mergeScanners(scanners: List[Scanner]): (Scanner, List[Vec3]) = {
        def _mergeScanners(current: Scanner, available: List[Scanner], used: Set[Scanner], offsets: List[Vec3]): (Scanner, List[Vec3]) = {
            logInfo(available.length.toString + " " + used.size.toString())
            if (used.size == scanners.length) (current, offsets)
            else {
                val next = available.filter(!used.contains(_)).headOption
                next match {
                    case Some(nextScanner) => {
                        val attempt = current.overlap(nextScanner, 0)
                        attempt match {
                            case Some((sc, offset)) => _mergeScanners(sc, available.filterNot(_ == nextScanner), used + nextScanner, offset :: offsets)
                            case _ => _mergeScanners(current, available.filterNot(_ == nextScanner), used, offsets)
                        }
                    }
                    case _ => _mergeScanners(current, scanners.filter(!used.contains(_)), used, offsets)
                }
            }
        }
        _mergeScanners(scanners.head, scanners.tail, Set(scanners.head), List())
    }

    override def part1(input: List[Scanner]): Unit = {
        logInfo("Starting part 1")
        logInfo("Num scanners = " + input.length)
        val finalScan = mergeScanners(input)
        logInfo(finalScan._1.beacons.length.toString())
    }

    override def part2(input: List[Scanner]): Unit = {
        logInfo("Starting part 2")
        logInfo("Num scanners = " + input.length)
        val finalScan = mergeScanners(input)
        val offsets = finalScan._2
        val maxDistance = (for (
            i <- 0 to offsets.length - 1;
            j <- i+1 to offsets.length - 1
            ) yield {
                val v = offsets(i) - offsets(j)
                v.x + v.y + v.z
            }
        ).max
        logInfo("Max distance = " + maxDistance.toString())
    }
}
