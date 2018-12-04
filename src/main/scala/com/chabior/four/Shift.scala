package com.chabior.four

import org.joda.time.{DateTime, Minutes}

case class Shift(guardId: Int, start: DateTime) {
    var sleeps: List[(DateTime, DateTime)] = List.empty

    def addSleep(sleep: (DateTime, DateTime)) = {
      sleeps = sleeps :+ sleep
    }

    def sleepDuration: Int = {
      var duration: Int = 0
      for (sleep <- sleeps) {
        duration = duration + Minutes.minutesBetween(sleep._1, sleep._2).getMinutes
      }
      duration
    }

  def sleepMinutes: List[Int] = {
    var minutes: List[Int] = List.empty
    for (sleep <- sleeps) {
      minutes = minutes ::: (sleep._1.minuteOfHour().get() to sleep._2.minuteOfHour().get()).toList
    }
    minutes
  }
}
