package com.chabior.seven

object App {
  def main(args: Array[String]): Unit = {
    val input = InputParser.parse
    println(input)
    println(NodeAnalyze.getPathMulti(input, List(new Worker, new Worker, new Worker, new Worker, new Worker)))
//    println(NodeAnalyze.getPathMulti(input, List(0, 2, 1, 3, 4)))
//    println(NodeAnalyze.getPath(input).mkString)
  }
}
