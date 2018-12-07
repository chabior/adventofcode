package com.chabior.seven

object App {
  def main(args: Array[String]): Unit = {
    val input = InputParser.parse
    println(input)
    println(NodeAnalyze.getPath(input).mkString)
  }
}
