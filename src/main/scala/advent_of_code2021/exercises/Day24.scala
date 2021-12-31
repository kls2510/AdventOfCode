package advent_of_code2021.exercises

import util._
import scala.util.Try

object Instruction {
    def stringToInstruction(inp: String): Instruction = {
        val words = inp.split(" ") 
        val lhs = Variable(words(1))
        if (words.head == "inp") Input(lhs) else {
            val rhs = if (Try(Integer.parseInt(words(2))).isSuccess) Number(Integer.parseInt(words(2)).toLong) else Variable(words(2))
            words.head match {
                case "add" => Add(lhs, rhs)
                case "mul" => Mul(lhs, rhs)
                case "div" => Div(lhs, rhs)
                case "mod" => Mod(lhs, rhs)
                case "eql" => Eq(lhs, rhs)
            }
        }
    }
}

trait Instruction

trait Value {
    def get(alu: ALU): Long;
}

case class Variable(name: String) extends Value {
    def set(alu: ALU, v: Long): ALU = ALU(alu.variables + (name -> v))
    def get(alu: ALU): Long = alu.variables(name)
}

case class Number(value: Long = 0) extends Value {
    def get(alu: ALU): Long = value
}

// inp a - Read an input value and write it to variable a.
case class Input(variable: Variable) extends Instruction

// add a b - Add the value of a to the value of b, then store the result in variable a.
case class Add(variable: Variable, number: Value) extends Instruction

// mul a b - Multiply the value of a by the value of b, then store the result in variable a.
case class Mul(variable: Variable, number: Value) extends Instruction

// div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
case class Div(variable: Variable, number: Value) extends Instruction

// mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
case class Mod(variable: Variable, number: Value) extends Instruction

// eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.
case class Eq(variable: Variable, number: Value) extends Instruction

case class ALU(variables: Map[String, Long] = Map(
    "w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> 0L
)) extends WithLogger {

    def process(instruction: Instruction, input: List[Long]): (ALU, List[Long]) = {
        instruction match {
            case Input(variable) => (variable.set(this, input.head), input.tail)
            case Add(variable, number) => (variable.set(this, variable.get(this) + number.get(this)), input)
            case Mul(variable, number) => (variable.set(this, variable.get(this) * number.get(this)), input)
            case Div(variable, number) => (variable.set(this, variable.get(this) / number.get(this)), input)
            case Mod(variable, number) => (variable.set(this, variable.get(this) % number.get(this)), input)
            case Eq(variable, number) => (variable.set(this, if (variable.get(this) == number.get(this)) 1 else 0), input)
        }
    }

    def runProgram(instructions: List[Instruction], inputs: List[Long]): ALU = {
        def stepProgram(currentAlu: ALU, _instructions: List[Instruction], _inputs: List[Long]): ALU = {
            _instructions match {
                case i :: rest => {
                    val (nextAlu, nextInputs) = currentAlu.process(i, _inputs)
                    stepProgram(nextAlu, rest, nextInputs)
                }
                case Nil => currentAlu
            }
        }
        stepProgram(this, instructions, inputs)
    }

}

object Day24 extends WithLogger with Exercise[List[Instruction]] {

    val dayNumber: Int = 24

    override def processRawLines(input: List[String]): List[Instruction] = input.map(Instruction.stringToInstruction(_))

    def genCandidates(): Iterator[List[Long]] = {
        (for (
            n0 <- (9 to 9).reverse.iterator;
            n1 <- (2 to 9).reverse.iterator;
            n2 <- (1 to 3).reverse.iterator;
            n4 <- (1 to 4).reverse.iterator;
            n5 <- (1 to 8).reverse.iterator;
            n9 <- (3 to 9).reverse.iterator;
            n10 <- (1 to 1).reverse.iterator
         ) yield {
            List(n0, n1, n2, n2 + 6, n4, n5, n5 + 1, n4 + 5, n1 - 1, n9, n10, n10 + 8, n9 - 2, n0 - 8)
        })
    }

    override def part1(input: List[Instruction]): Unit = {
        logInfo("Starting part 1")
        val alu = ALU()
        val numbersToCheck = genCandidates()
        val highestNumber = numbersToCheck.zipWithIndex.find{case (num, i) => {
            val output = alu.runProgram(input, num)
            output.variables("z") == 0
        }}
        logInfo(highestNumber.get._1.mkString)
    }

    def genCandidates2(): Iterator[List[Long]] = {
        (for (
            n0 <- (9 to 9).iterator;
            n1 <- (2 to 9).iterator;
            n2 <- (1 to 3).iterator;
            n4 <- (1 to 4).iterator;
            n5 <- (1 to 8).iterator;
            n9 <- (3 to 9).iterator;
            n10 <- (1 to 1).iterator
         ) yield {
            List(n0, n1, n2, n2 + 6, n4, n5, n5 + 1, n4 + 5, n1 - 1, n9, n10, n10 + 8, n9 - 2, n0 - 8)
        })
    }

    override def part2(input: List[Instruction]): Unit = {
        logInfo("Starting part 2")
        val alu = ALU()
        val numbersToCheck = genCandidates2()
        val lowestNumber = numbersToCheck.zipWithIndex.find{case (num, i) => {
            val output = alu.runProgram(input, num)
            output.variables("z") == 0
        }}
        logInfo(lowestNumber.get._1.mkString)
    }
}
