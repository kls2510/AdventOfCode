package advent_of_code2021.exercises

import util._

trait SnailNode {
    var parent: Option[ParentNode] = None
    def stringify(): String;
    def magnitude(): Int;
}

class ParentNode extends SnailNode {
    var child1: SnailNode = new LeafNode()
    var child2: SnailNode = new LeafNode()
    def stringify(): String = "[" + child1.stringify + "," + child2.stringify + "]"
    def magnitude(): Int = 3 * child1.magnitude + 2 * child2.magnitude
}

class LeafNode extends SnailNode {
    var value: Int = -1
    def stringify(): String = value.toString
    def magnitude(): Int = value
}

object SnailGraph extends WithLogger {
    def parseGraph(input: String): SnailGraph = {
        def _getNode(input: String, parent: Option[ParentNode]): SnailNode = {
            if (input.startsWith("[")) {
                val newParent = new ParentNode()
                newParent.child1 = _getNode(input.tail, Some(newParent))
                newParent.child2 = _getNode(input.drop(newParent.child1.stringify.length + 2), Some(newParent))
                newParent.parent = parent
                newParent
            } else {
                val leaf = new LeafNode()
                leaf.value = Integer.parseInt(input.head.toString)
                leaf.parent = parent
                leaf
            }
        }
        SnailGraph(_getNode(input, None).asInstanceOf[ParentNode])
    }
}

case class SnailGraph(rootNode: ParentNode) extends WithLogger{
    def print() = logInfo(rootNode.stringify)

    def magnitude(): Int = rootNode.magnitude

    def +(other: SnailGraph): SnailGraph = {
        val newRoot = new ParentNode()
        newRoot.child1 = rootNode
        newRoot.child2 = other.rootNode
        rootNode.parent = Some(newRoot)
        other.rootNode.parent = Some(newRoot)
        val newGraph = SnailGraph(newRoot)
        newGraph.reduceNumber
        newGraph
    }

    def reduceNumber(): Unit = {
        def _reduceNumber(previousState: String): Unit = {
            balanceStep
            val newState = rootNode.stringify
            if (newState != previousState) _reduceNumber(newState)
        }
        _reduceNumber(rootNode.stringify)
    }

    def balanceStep(): Unit = {
        findExplode.map(explode(_)).orElse(findSplit.map(split(_)))
    }

    def findExplode(): Option[ParentNode] = {
        def _findExplode(start: ParentNode, depth: Int): Option[ParentNode] = {
            if (depth == 4) {
                Some(start)
            } else {
                val nextDepth = depth + 1
                (start.child1, start.child2) match {
                    case (c1: ParentNode, c2: ParentNode)  => {
                        val maybeLeft = _findExplode(c1, nextDepth)
                        maybeLeft match {
                            case None => _findExplode(c2, nextDepth)
                            case _ => maybeLeft
                        }
                    }
                    case (c1: ParentNode, c2: LeafNode)  => _findExplode(c1, nextDepth)
                    case (c1: LeafNode, c2: ParentNode)  => _findExplode(c2, nextDepth)
                    case _ => None
                }
            }
        }
        _findExplode(rootNode, 0)
    }

    def findRightMostLeaf(start: SnailNode): Option[LeafNode] = {
        def _findRightmostLeaf(current: SnailNode): Option[LeafNode] = {
            current match {
                case l: LeafNode => Some(l)
                case p: ParentNode => {
                    val maybeRight = _findRightmostLeaf(p.child2)
                    maybeRight match {
                        case None => _findRightmostLeaf(p.child1)
                        case _ => maybeRight
                    }
                }
            }
        }
        _findRightmostLeaf(start)
    }

    def findLeafToLeft(start: ParentNode): Option[LeafNode] = {
        def _findLeafToLeft(current: ParentNode, previous: ParentNode): Option[LeafNode] = {
            (current.child1) match {
                case c1: LeafNode => Some(c1)
                case c1: ParentNode if c1 == previous => {
                    if (current.parent.isDefined) _findLeafToLeft(current.parent.get, current) else None
                }
                case c1: ParentNode  => findRightMostLeaf(c1)
            }
        }
        _findLeafToLeft(start.parent.get, start)
    }

    def findLeftMostLeaf(start: SnailNode): Option[LeafNode] = {
        def _findLeftmostLeaf(current: SnailNode): Option[LeafNode] = {
            current match {
                case l: LeafNode => Some(l)
                case p: ParentNode => {
                    val maybeLeft = _findLeftmostLeaf(p.child1)
                    maybeLeft match {
                        case None => _findLeftmostLeaf(p.child2)
                        case _ => maybeLeft
                    }
                }
            }
        }
        _findLeftmostLeaf(start)
    }

    def findLeafToRight(start: ParentNode): Option[LeafNode] = {
        def _findLeafToRight(current: ParentNode, previous: ParentNode): Option[LeafNode] = {
            (current.child2) match {
                case c2: LeafNode => Some(c2)
                case c2: ParentNode if c2 == previous => {
                    if (current.parent.isDefined) _findLeafToRight(current.parent.get, current) else None
                }
                case c2: ParentNode  => findLeftMostLeaf(c2)
            }
        }
        _findLeafToRight(start.parent.get, start)
    }

    def explode(explodeNode: ParentNode): Unit = {
        // add to next number to left
        val leftVal = explodeNode.child1.asInstanceOf[LeafNode].value
        val valueToLeft = findLeafToLeft(explodeNode)
        valueToLeft.foreach(leaf => leaf.value = leaf.value + leftVal)
        // add next number to right
        val rightVal = explodeNode.child2.asInstanceOf[LeafNode].value
        val valueToRight = findLeafToRight(explodeNode)
        valueToRight.foreach(leaf => leaf.value = leaf.value + rightVal)
        // make parent point to value 0
        val parent = explodeNode.parent.get
        val newNode = new LeafNode()
        newNode.value = 0
        newNode.parent = Some(parent)
        if (parent.child1 == explodeNode.asInstanceOf[SnailNode]) parent.child1 = newNode else parent.child2 = newNode
    }

    def findSplit(): Option[LeafNode] = {
        def _findSplit(start: SnailNode): Option[LeafNode] = {
            start match {
                case l : LeafNode => if (l.value >= 10) {
                    Some(start.asInstanceOf[LeafNode])
                } else None
                case p: ParentNode => {
                    val maybeLeft = _findSplit(p.child1)
                    maybeLeft match {
                        case None => _findSplit(p.child2)
                        case _ => maybeLeft
                    }
                }
            }
        }
        _findSplit(rootNode)
    }

    def split(splitNode: LeafNode): Unit = {
        val newParent = new ParentNode()
        newParent.parent = splitNode.parent

        val grandparent = splitNode.parent.get.asInstanceOf[ParentNode]
        if (grandparent.child1 == splitNode) grandparent.child1 = newParent else grandparent.child2 = newParent

        val leftLeaf = new LeafNode()
        leftLeaf.value = math.floor(splitNode.value / 2f).toInt
        leftLeaf.parent = Some(newParent)
        val rightLeaf = new LeafNode()
        rightLeaf.value = math.ceil(splitNode.value / 2f).toInt
        rightLeaf.parent = Some(newParent)
        
        newParent.child1 = leftLeaf
        newParent.child2 = rightLeaf
    }
}


object Day18 extends WithLogger with Exercise[List[String]] {

    val dayNumber: Int = 18

    override def processRawLines(input: List[String]): List[String] = input

    override def part1(input: List[String]): Unit = {
        val graphs = input.map(SnailGraph.parseGraph(_))
        logInfo("Starting part 1")
        val sum = graphs.reduce(_ + _)
        sum.print
        logInfo("Magnitude = " + sum.magnitude)
    }

    override def part2(input: List[String]): Unit = {
        logInfo("Starting part 2")

        val m = (for(
            i <- 0 to input.length - 1;
            j <- i+1 to input.length - 1
        ) yield {
            val r1 = SnailGraph.parseGraph(input(i)) + SnailGraph.parseGraph(input(j))
            val r2 = SnailGraph.parseGraph(input(j)) + SnailGraph.parseGraph(input(i))
            List(r1.magnitude, r2.magnitude)
        }).flatten.max
        logInfo("Max magnitude = " + m)
    }
}
