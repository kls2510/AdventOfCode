package advent_of_code2021.exercises

import collection.mutable.PriorityQueue

import util._

object Hallway extends WithLogger {
    val energies = Map(
        'A' -> 1,
        'B' -> 10,
        'C' -> 100,
        'D' -> 1000,
    )

    val rooms = Map(
        'A' -> 0,
        'B' -> 1,
        'C' -> 2,
        'D' -> 3,
    )

    def isInHall(amphiLoc: (Int, Int)): Boolean = amphiLoc._1 == 1

    val allValidHallSpaces: List[(Int, Int)] = List(
        1, 2, 4, 6, 8, 10, 11
    ).map((1, _))

    def getRoomNo(amphiLoc: (Int, Int)): Option[Int] = amphiLoc match {
        case (_, 3) => Some(0)
        case (_, 5) => Some(1)
        case (_, 7) => Some(2)
        case (_, 9) => Some(3)
        case _ => None
    }

    def coordFromRoomAndPos(room: Int, front: Boolean): (Int, Int) = {
        val col = room match {
            case 0 => 3
            case 1 => 5
            case 2 => 7
            case 3 => 9
        }
        val row = if (front) 2 else 3
        (row, col)
    }

    def buildPath(finalState: Hallway, parents: Map[Hallway, Hallway]): List[Hallway] = {
        def _build(currentState: Hallway, parents: Map[Hallway, Hallway]): List[Hallway] = {
            parents.get(currentState) match {
                case Some(parent) => currentState :: _build(parent, parents)
                case None => List(currentState)
            }
        }
        _build(finalState, parents).reverse
    }

    def runUntilComplete(activeSet: PriorityQueue[(Hallway, Int)], scores: Map[Hallway, (Int, Boolean)]): (Int, List[Hallway]) = {
        var pq = activeSet
        def Dijkstra(_scores: Map[Hallway, (Int, Boolean)], parents: Map[Hallway, Hallway]): (Int, List[Hallway]) = {
            if (pq.isEmpty) (Integer.MAX_VALUE, List()) else {
                val bestNextStep = pq.dequeue
                val nextState = bestNextStep._1
                val currentRealScore = _scores(nextState)._1
                val validMoves = nextState.listAllMoves

                if (validMoves.size == 0 && nextState.diagram(1).filter(Hallway.energies.keySet.contains(_)).size == 0) {
                    logInfo("FOUND")
                    val finalScore = currentRealScore
                    val path = buildPath(nextState, parents)
                    (finalScore, path)
                }
                else {
                    val newScoresAndParents = validMoves.foldLeft((_scores + (nextState -> (currentRealScore, true)), parents)){
                        case ((allScores, allParents), (neighbourState, cost)) => {
                            val neighbourRealScore = currentRealScore + cost
                            val potentialNextEntry = (neighbourState -> (neighbourRealScore, false))
                            allScores.get(neighbourState) match {
                                case Some((_, true)) => (allScores, allParents)
                                case Some((previousRealScore, false)) => if (neighbourRealScore < previousRealScore) {
                                    pq.mapInPlace(e => if (e._1 == neighbourState) (e._1, neighbourRealScore) else e)
                                    pq = pq.clone
                                    (allScores + potentialNextEntry, allParents + (neighbourState -> nextState))
                                } else (allScores, allParents)
                                case None => {
                                    pq.enqueue((potentialNextEntry._1, potentialNextEntry._2._1))
                                    (allScores + potentialNextEntry, allParents + (neighbourState -> nextState))
                                }
                            }
                        }
                    }
                    Dijkstra(newScoresAndParents._1, newScoresAndParents._2)
                }
            }
        }
        Dijkstra(scores, Map())
    }
}

trait Hallway {
    val diagram: Vector[Vector[Char]]
    def print(): Unit;
    def listAllMoves(): Map[Hallway, Int]
    def getAmphiLocations(): List[(Int, Int)] = {
        diagram.flatten.zipWithIndex.foldLeft(List(): List[(Int, Int)]){
            case (allCoords, (c, index)) => if (Hallway.energies.keySet.contains(c)) {
                (index / diagram(0).length, index % diagram(0).length) :: allCoords
            } else allCoords
        }.reverse
    }
    def filterIfPathIsBlocked(amphiLoc: (Int, Int), allCandidates: List[(Int, Int)], entering: Boolean): List[(Int, Int)] = {
        allCandidates.filter(
            targetCoord => {
                val vertical = if (entering) List.range(1, targetCoord._1).map((_, targetCoord._2)) else List.range(1, amphiLoc._1).map((_, amphiLoc._2))
                val ys = List(amphiLoc._2, targetCoord._2)
                val pathAcross = List.range(ys.min, ys.max).map((1, _))
                val fullPath = (vertical ++ pathAcross ++ List(targetCoord)).distinct.filter(_ != amphiLoc)
                fullPath.map(step => {
                    !Hallway.energies.keySet.contains(diagram(step._1)(step._2))
                }).forall(identity)
        })
    }
}

case class Hallway1(diagram: Vector[Vector[Char]], completedMovement: Vector[(Boolean, Boolean)] = List.range(0, 4).map(_ => (false, false)).toVector) extends WithLogger with Hallway {

    def print(): Unit = {
        val topLines = diagram.take(2)
        val rooms = List.range(2, 4).map( r =>
            diagram(r).zipWithIndex.map{ case (c, i) => {
                val maybeRoom = Hallway.getRoomNo((r, i))
                maybeRoom match {
                    case Some(roomNo) => if (completedMovement(roomNo).toString().drop(1).dropRight(1).split(",")(r - 2) == "true") c.toLower else c
                    case _ => c
                }
            }}
        ).toVector
        val bottomLine = diagram.takeRight(1)
        (topLines ++ rooms ++ bottomLine).foreach(r => logInfo(r.mkString))
    }

    def getAllRoomContents(): Vector[(Option[Char], Option[Char])] = {
        def toOption(c: Char): Option[Char] = if (Hallway.energies.keySet.contains(c)) Some(c) else None
        val r1 = (toOption(diagram(2)(3)), toOption(diagram(3)(3)))
        val r2 = (toOption(diagram(2)(5)), toOption(diagram(3)(5)))
        val r3 = (toOption(diagram(2)(7)), toOption(diagram(3)(7)))
        val r4 = (toOption(diagram(2)(9)), toOption(diagram(3)(9)))
        Vector(r1, r2, r3, r4)
    }

    def genNewState(amphiLoc: (Int, Int), targetLoc: (Int, Int), markMovement: Boolean): (Hallway, Int) = {
        val amphiChar = diagram(amphiLoc._1)(amphiLoc._2)
        val distanceMoved = math.abs(targetLoc._1 - amphiLoc._1) + math.abs(targetLoc._2 - amphiLoc._2)
        val score = distanceMoved * Hallway.energies(amphiChar)
        val newDiagram = diagram.updated(targetLoc._1, diagram(targetLoc._1).updated(targetLoc._2, amphiChar))
        val newDiagram2 = newDiagram.updated(amphiLoc._1, newDiagram(amphiLoc._1).updated(amphiLoc._2, '.'))

        if (markMovement) {
            val room = Hallway.getRoomNo(targetLoc).get
            val newCompleted = completedMovement.updated(room, if (targetLoc._1 == 2) (true, true) else (false, true))
            (Hallway1(newDiagram2, newCompleted) -> score)
        } else (Hallway1(newDiagram2, completedMovement) -> score)
        
    }
    
    def listAllMoves(): Map[Hallway, Int] = {
        getAmphiLocations.foldLeft(Map(): Map[Hallway, Int])(
            (allPotentialMoves, amphiLoc) => {
                if (Hallway.isInHall(amphiLoc)) {
                    // move into room and mark as stopped
                    val allRooms = getAllRoomContents
                    val amphiChar = diagram(amphiLoc._1)(amphiLoc._2)
                    val targetRoom = Hallway.rooms(amphiChar)
                    val freeRoomSpace = allRooms(targetRoom) match {
                        case (None, None) => Some(Hallway.coordFromRoomAndPos(targetRoom, false))
                        case (None, Some(other)) if other == amphiChar => Some(Hallway.coordFromRoomAndPos(targetRoom, true))
                        case _ => None
                    }
                    val allPotentialTargets = filterIfPathIsBlocked(amphiLoc, freeRoomSpace.map(List(_)).getOrElse(List()), true)
                    val newEntries = allPotentialTargets.map(targetLoc => genNewState(amphiLoc, targetLoc, true)).toMap
                    allPotentialMoves ++ newEntries
                } else {
                    val roomNo = Hallway.getRoomNo(amphiLoc).get
                    val completedRoomState = completedMovement(roomNo)
                    val completedAmphiState = if (amphiLoc._1 == 2) completedRoomState._1 else completedRoomState._2
                    if (completedAmphiState) allPotentialMoves else {
                        // attempt move out of room
                        val allPotentialTargets = filterIfPathIsBlocked(amphiLoc, Hallway.allValidHallSpaces, false)
                        val newEntries = allPotentialTargets.map(targetLoc => genNewState(amphiLoc, targetLoc, false)).toMap
                        allPotentialMoves ++ newEntries
                    }
                } 
            }
        )
    }
}

case class Hallway2(diagram: Vector[Vector[Char]], completedMovement: Vector[(Boolean, Boolean, Boolean, Boolean)] = List.range(0, 4).map(_ => (false, false, false, false)).toVector) extends WithLogger with Hallway {

    def print(): Unit = {
        val topLines = diagram.take(2)
        val rooms = List.range(2, 6).map( r =>
            diagram(r).zipWithIndex.map{ case (c, i) => {
                val maybeRoom = Hallway.getRoomNo((r, i))
                maybeRoom match {
                    case Some(roomNo) => if (completedMovement(roomNo).toString().drop(1).dropRight(1).split(",")(r - 2) == "true") c.toLower else c
                    case _ => c
                }
            }}
        ).toVector
        val bottomLine = diagram.takeRight(1)
        (topLines ++ rooms ++ bottomLine).foreach(r => logInfo(r.mkString))
    }

    def getAllRoomContents(): Vector[(Option[Char], Option[Char], Option[Char], Option[Char])] = {
        def toOption(c: Char): Option[Char] = if (Hallway.energies.keySet.contains(c)) Some(c) else None
        val r1 = (toOption(diagram(2)(3)), toOption(diagram(3)(3)), toOption(diagram(4)(3)), toOption(diagram(5)(3)))
        val r2 = (toOption(diagram(2)(5)), toOption(diagram(3)(5)), toOption(diagram(4)(5)), toOption(diagram(5)(5)))
        val r3 = (toOption(diagram(2)(7)), toOption(diagram(3)(7)), toOption(diagram(4)(7)), toOption(diagram(5)(7)))
        val r4 = (toOption(diagram(2)(9)), toOption(diagram(3)(9)), toOption(diagram(4)(9)), toOption(diagram(5)(9)))
        Vector(r1, r2, r3, r4)
    }

    def genNewState(amphiLoc: (Int, Int), targetLoc: (Int, Int), markMovement: Boolean): (Hallway, Int) = {
        val amphiChar = diagram(amphiLoc._1)(amphiLoc._2)
        val distanceMoved = math.abs(targetLoc._1 - amphiLoc._1) + math.abs(targetLoc._2 - amphiLoc._2)
        val score = distanceMoved * Hallway.energies(amphiChar)
        val newDiagram = diagram.updated(targetLoc._1, diagram(targetLoc._1).updated(targetLoc._2, amphiChar))
        val newDiagram2 = newDiagram.updated(amphiLoc._1, newDiagram(amphiLoc._1).updated(amphiLoc._2, '.'))

        if (markMovement) {
            val room = Hallway.getRoomNo(targetLoc).get
            val newCompleted = completedMovement.updated(room, if (targetLoc._1 == 2) (true, true, true, true) 
                else if (targetLoc._1 == 3) (false, true, true, true)
                else if (targetLoc._1 == 4) (false, false, true, true)
                else (false, false, false, true)
            )
            (Hallway2(newDiagram2, newCompleted) -> score)
        } else (Hallway2(newDiagram2, completedMovement) -> score)
        
    }
    
    def listAllMoves(): Map[Hallway, Int] = {
        getAmphiLocations.foldLeft(Map(): Map[Hallway, Int])(
            (allPotentialMoves, amphiLoc) => {
                if (Hallway.isInHall(amphiLoc)) {
                    // move into room and mark as stopped
                    val allRooms = getAllRoomContents
                    val amphiChar = diagram(amphiLoc._1)(amphiLoc._2)
                    val targetRoom = Hallway.rooms(amphiChar)
                    val targetCol = Hallway.coordFromRoomAndPos(targetRoom, true)._2
                    val freeRoomSpace = allRooms(targetRoom) match {
                        case (None, None, None, None) => Some((5, targetCol))
                        case (None, None, None, Some(other)) if other == amphiChar => Some((4, targetCol))
                        case (None, None, Some(other), Some(_)) if other == amphiChar => Some((3, targetCol))
                        case (None, Some(other), Some(_), Some(_)) if other == amphiChar => Some((2, targetCol))
                        case _ => None
                    }
                    val allPotentialTargets = filterIfPathIsBlocked(amphiLoc, freeRoomSpace.map(List(_)).getOrElse(List()), true)
                    val newEntries = allPotentialTargets.map(targetLoc => genNewState(amphiLoc, targetLoc, true)).toMap
                    allPotentialMoves ++ newEntries
                } else {
                    // logInfo(amphiLoc.toString())
                    val roomNo = Hallway.getRoomNo(amphiLoc).get
                    val completedRoomState = completedMovement(roomNo)
                    val completedAmphiState = if (amphiLoc._1 == 2) completedRoomState._1
                        else if (amphiLoc._1 == 3) completedRoomState._2
                        else if (amphiLoc._1 == 4) completedRoomState._3
                        else completedRoomState._4
                    if (completedAmphiState) allPotentialMoves else {
                        // attempt move out of room
                        val allPotentialTargets = filterIfPathIsBlocked(amphiLoc, Hallway.allValidHallSpaces, false)
                        val newEntries = allPotentialTargets.map(targetLoc => genNewState(amphiLoc, targetLoc, false)).toMap
                        allPotentialMoves ++ newEntries
                    }
                } 
            }
        )
    }
}


object Day23 extends WithLogger with Exercise[Hallway] {

    val dayNumber: Int = 23

    override def processRawLines(input: List[String]): Hallway = Hallway1(input.map(_.toCharArray.toVector).toVector)

    override def part1(input: Hallway): Unit = {
        logInfo("Starting part 1")
        val (cost, path) = Hallway.runUntilComplete(PriorityQueue((input, 0)){ case (e1, e2) => if (e1._2 > e2._2) -1 else 1 }, Map(input -> (0, false)))
        logInfo("Score = " + cost)
        path.foreach(_.print())
    }

    override def part2(input: Hallway): Unit = {
        logInfo("Starting part 2")
        val extra = Vector(
            Vector(' ', ' ', '#', 'D', '#', 'C', '#', 'B', '#', 'A', '#', ' ', ' '),
            Vector(' ', ' ', '#', 'D', '#', 'B', '#', 'A', '#', 'C', '#', ' ', ' ')
        )
        val original = input.diagram
        val padding = Vector(' ', ' ')
        val newDiagram = original.take(3) ++ extra ++ Vector(original(3) ++ padding) ++ original.takeRight(2).map(_ ++ padding)
        val newInput = Hallway2(newDiagram)
        val (cost, path) = Hallway.runUntilComplete(PriorityQueue((newInput.asInstanceOf[Hallway], 0)){ case (e1, e2) => if (e1._2 > e2._2) -1 else 1 }, Map(newInput -> (0, false)))
        logInfo("Score = " + cost)
        path.foreach(_.print())
    }
}
