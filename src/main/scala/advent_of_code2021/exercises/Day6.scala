package advent_of_code2021.exercises

import util._


object Fish {
    val gestationPeriod = 6
    val delay = 2
}

object Population {
    def fromOriginalPopulation(fishes: List[Int]): Population = Population(fishes.groupMapReduce(identity)(_ => 1L)(_ + _))
}

case class Population(pop: Map[Int, Long]) extends WithLogger{
    def nextTimePeriod(): Population = {
        val newDayGestation =  pop.getOrElse(0, 0L) + pop.getOrElse(Fish.gestationPeriod + 1, 0L)
        val decrementedPop = pop.map(entry => if (entry._1 > 0) ((entry._1 - 1) -> entry._2) else ((Fish.gestationPeriod + Fish.delay) -> entry._2))
        Population(decrementedPop + (Fish.gestationPeriod -> newDayGestation))
    }
}

object Day6 extends WithLogger with Exercise[List[Int]] {

    val dayNumber: Int = 6

    override def processRawLines(input: List[String]): List[Int] = {
        input match {
            case s :: Nil => s.split(",").map(i => i.toInt).toList
        }
    }

    override def part1(input: List[Int]): Unit = {
        logInfo("Starting part 1")

        val simLength: Int = 80
        val fishes = List.range(0, simLength).foldLeft(Population.fromOriginalPopulation(input))(
            (currentPop, timestep) => currentPop.nextTimePeriod
        )

        logInfo("Num fish = " + fishes.pop.values.reduce(_ + _).toString)
    }

    override def part2(input: List[Int]): Unit = {
        logInfo("Starting part 2")

        val simLength: Int = 256
        val fishes = List.range(0, simLength).foldLeft(Population.fromOriginalPopulation(input))(
            (currentPop, timestep) => currentPop.nextTimePeriod
        )

        logInfo("Num fish = " + fishes.pop.values.reduce(_ + _).toString)
    }
}
