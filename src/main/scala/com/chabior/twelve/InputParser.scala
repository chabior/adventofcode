package com.chabior.twelve

import scala.io.Source

object InputParser {
  def parse: Int = {
    var state = Source.fromResource("twelve_initial_state").getLines.toList.head.grouped(1).zipWithIndex.map(x => x._2 -> x._1).toMap

    val patterns = Source.fromResource("twelve_patterns").getLines.map(x => {
      val p = "([#.]+) => ([#.]+)".r
      val p(pattern, hasPlant) = x

      (pattern, hasPlant)
    }).filter(x => x._2 == "#").map(x => x._1).toList

    def mutate(state: Map[Int, String]): Map[Int, String] = {
      var newState = Map.empty[Int, String]
      val min = state.keys.toList.min - 3
      val max = state.keys.toList.max + 4
      for (x <- min to max) {
        val pattern = state.getOrElse(x - 2, ".") + state.getOrElse(x - 1, ".") + state.getOrElse(x, ".") + state.getOrElse(x + 1, ".") + state.getOrElse(x + 2, ".")
        val d = patterns.find(x => x == pattern) match {
          case None => "."
          case Some(_) => "#"
        }
        newState = newState + (x -> d)
      }
      newState
    }
    var ls = 0
    var s = 0
    for (i <- 1 to 2000) {
      state = mutate(state)
      s = state.filter(x => x._2 == "#").keys.sum
      ls = state.filter(x => x._2 == "#").keys.sum
    }
    println(s)
    println(ls)
    println((50000000000L - 2000L) * 59L + 119598L)
    1
  }

  def main(args: Array[String]): Unit = {
    parse
  }
}
