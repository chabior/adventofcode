package com.chabior.five

object App {
  def main(args: Array[String]): Unit = {
    println(Reactor.lengthOfBest(InputParser.parse))
  }
}
