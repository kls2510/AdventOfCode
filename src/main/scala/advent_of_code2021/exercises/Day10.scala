package advent_of_code2021.exercises

import util._


object Stack {
    def newStack(): Stack = Stack(List())
}

case class Stack(contents: List[Char]) {
    def push(element: Char): Stack = Stack(element :: contents)
    def peek: Option[Char] = contents.headOption
    def pop: (Option[Char], Stack) = (contents.headOption, Stack(contents.tail))
}

object Day10 extends WithLogger with Exercise[List[List[Char]]] {

    val dayNumber: Int = 10
    val brackets = Map(
        '(' -> ')',
        '[' -> ']',
        '{' -> '}',
        '<' -> '>',
    )
    val opens = brackets.keySet.toList
    val closes = brackets.values.toList
    val syntaxScore = Map(
        ')' -> 3,
        ']' -> 57,
        '}' -> 1197,
        '>' -> 25137,
    )
    val autoScore = Map(
        ')' -> 1,
        ']' -> 2,
        '}' -> 3,
        '>' -> 4,
    )

    override def processRawLines(input: List[String]): List[List[Char]] = {
        input.foldLeft(List(): List[List[Char]]){
            (allLines, line) => line.toCharArray.toList :: allLines
        }.reverse
    }

    def runStack(line: List[Char]): (Stack, Option[Char]) = {
        line.foldLeft(Stack.newStack, None: Option[Char]){
            case ((stack, illegalChar), nextChar) => {
                if (illegalChar.isDefined) (stack, illegalChar) else {
                    (stack.peek, nextChar) match {
                        case (_, next) if opens.contains(next) => (stack.push(next), None)
                        case (Some(open), next) if brackets(open) == next => (stack.pop._2, None)
                        case (_, next) => (stack, Some(next))
                    }
                }
            }
        }
    }

    override def part1(input: List[List[Char]]): Unit = {
        logInfo("Starting part 1")
        val mismatchedBrackets = input.foldLeft(List(): List[Option[Char]]){
            (allMismatched, l) => {
                val result = runStack(l)
                result._2 :: allMismatched
            }
        }
        val score = mismatchedBrackets.map(_ match {
            case Some(bracket) => syntaxScore(bracket)
            case _ => 0
        }).reduce(_ + _)
        logInfo("Syntax score = " + score.toString)
    }

    override def part2(input: List[List[Char]]): Unit = {
        logInfo("Starting part 2")

        val missingBrackets = input.foldLeft(List(): List[Option[List[Char]]]){
            (allMissing, l) => {
                val result = runStack(l)
                (result match {
                    case (_, Some(bracket)) => None
                    case (stack, None) if stack.peek.isEmpty => None
                    case (stack, _) => Some(stack.contents.map(brackets(_)))
                }) :: allMissing
            }
        }.filter(_.isDefined).map(_.get)
        val scores = missingBrackets.map(
            _.foldLeft(0L)((score, next) => score * 5 + autoScore(next))
        ).sorted
        logInfo("Scores = " + scores.toString)
        val middle = scores.length / 2
        logInfo("Length = " + scores.length.toString)
        logInfo("Middle = " + middle.toString)
        val middleScore = scores(middle)
        logInfo("Middle autocomplete score = " + middleScore.toString)
    }
}
