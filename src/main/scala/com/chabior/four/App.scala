package com.chabior.four

object App {
  def main(args: Array[String]): Unit = {
    println(ShiftManager.findMostSleepMinute(InputParser.parse("security_journal")))
  }
}
